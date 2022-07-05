/*** includes ***/

#define _DEFAULT_SOURCE
#define _BSD_SOURCE
#define _GNU_SOURCE

#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <wchar.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>
#include <locale.h>

/*** defines ***/

#define KILO_VERSION "0.0.1"
#define KILO_TAB_STOP 4
#define KILO_QUIT_TIMES 3

#define CTRL_KEY(k) ((k) & 0x1f)
#define IS_TAB(k) (k == '\t')
#define ESC_KEY '\x1b'

enum editorKey {
    BACKSPACE = 127,
    ARROW_LEFT = 1000,
    ARROW_RIGHT,
    ARROW_UP,
    ARROW_DOWN,
    DEL_KEY,
    HOME_KEY,
    END_KEY,
    PAGE_UP,
    PAGE_DOWN
};

enum editorHighlight {
    HL_NORMAL = 0,
    HL_COMMENT,
    HL_MLCOMMENT,
    HL_KEYWORD1,
    HL_KEYWORD2,
    HL_STRING,
    HL_NUMBER,
    HL_MATCH
};

#define HL_HIGHLIGHT_NUMBERS (1<<0)
#define HL_HIGHLIGHT_STRINGS (1<<1)

#define NORMAL_MODE 1
#define INSERT_MODE 0

/*** data ***/
struct cursor {
    int col;
    int row;
};

struct editorSyntax {
    char *filetype;
    char **filematch;
    char **keywords;
    char *singleline_comment_start;
    char *multiline_comment_start;
    char *multiline_comment_end;
    char *indent_chars;
    char *unindent_chars;
    int flags;
};

typedef struct erow {
    int idx;
    int size;
    int rsize;
    char *chars;
    char *render;
    unsigned char *hl;
    int hl_open_comment;
} erow;

struct editorConfig {
    int cx, cy;
    int rx;
    int rowoff;
    int coloff;
    int screenrows;
    int screencols;
    int numrows;
    erow *row;
    int dirty;
    int leftpad;
    char *filename;
    char statusmsg[80];
    time_t statusmsg_time;
    struct editorSyntax *syntax;
    struct termios orig_termios;
    int mode;
    char expandtab;
};

struct cursor prev_cursor;
struct editorConfig E;

/*** filetypes ***/

char *C_HL_extensions[] = { ".c", ".h", ".cpp", NULL };
char *C_HL_keywords[] = {
    "switch", "if", "while", "for", "break", "continue", "return", "else",
    "struct", "union", "typedef", "static", "enum", "class", "case",

    "int|", "long|", "double|", "float|", "char|", "unsigned|", "signed|",
    "void|", NULL
};

struct editorSyntax HLDB[] = {
    {
        "c",
        C_HL_extensions,
        C_HL_keywords,
        "//", "/*", "*/",
        "{[", "}]",
        HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS
    },
};

#define HLDB_ENTRIES (sizeof(HLDB) / sizeof(HLDB[0]))

/*** prototypes ***/

void editorSetStatusMessage(const char *fmt, ...);
void editorRefreshScreen();
void restoreCursorPosition();
void saveCursorPosition();
int getCursorPosition(int *row, int *col);
char *editorPrompt(char *prompt, void (*callback)(char *, int));
int numDigits(int num);

/*** terminal ***/

void die(const char *s) {
    write(STDOUT_FILENO, "\x1b[2J", 4);
    write(STDOUT_FILENO, "\x1b[H", 3);

    perror(s);
    exit(1);
}

void saveCursorPosition() {
    getCursorPosition(&prev_cursor.row, &prev_cursor.col);
}

void restoreCursorPosition() {
    char buf[32];
    int len = snprintf(
        buf, sizeof(buf), "\x1b[%d;%dH", prev_cursor.row, prev_cursor.col
    );
    write(STDOUT_FILENO, buf, len);
}

void disableRawMode() {
    write(STDOUT_FILENO, "\033[?47l", 6);
    restoreCursorPosition();

    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &E.orig_termios) == -1) {
        die("tcsetattr");
    }
}

void enableRawMode() {
    if (tcgetattr(STDIN_FILENO, &E.orig_termios) == -1) {
        die("tcgetattr");
    }

    atexit(disableRawMode);

    struct termios raw = E.orig_termios;
    raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
    raw.c_oflag &= ~(OPOST);
    raw.c_cflag |= (CS8);
    raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
    raw.c_cc[VMIN] = 0;
    raw.c_cc[VTIME] = 1;

    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == -1) {
        die("tcsetattr");
    }
}

int editorReadKey() {
    int nread;
    char c;

    while ((nread = read(STDIN_FILENO, &c, 1)) != 1) {
        if (nread == -1 && errno != EAGAIN) die("read");
    }

    if (c != ESC_KEY) {
        return c;
    }

    char seq[3];

    if (read(STDIN_FILENO, &seq[0], 1) != 1) return ESC_KEY;
    if (read(STDIN_FILENO, &seq[1], 1) != 1) return ESC_KEY;

    if (seq[0] == '[') {
        if (seq[1] >= '0' && seq[1] <= '9') {
            if (read(STDIN_FILENO, &seq[2], 1) != 1) return ESC_KEY;
            if (seq[2] == '~') {
                switch (seq[1]) {
                    case '1': return HOME_KEY;
                    case '3': return DEL_KEY;
                    case '4': return END_KEY;
                    case '5': return PAGE_UP;
                    case '6': return PAGE_DOWN;
                    case '7': return HOME_KEY;
                    case '8': return END_KEY;
                }
            }
        } else {
            switch (seq[1]) {
                case 'A': return ARROW_UP;
                case 'B': return ARROW_DOWN;
                case 'C': return ARROW_RIGHT;
                case 'D': return ARROW_LEFT;
                case 'H': return HOME_KEY;
                case 'F': return END_KEY;
            }
        }
    } else if (seq[0] == 'O') {
        switch (seq[1]) {
            case 'H': return HOME_KEY;
            case 'F': return END_KEY;
        }
    }

    return ESC_KEY;
}

int ishexdigit(char c) {
    return isdigit(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F');
}

int getCursorPosition(int *rows, int *cols) {
    char buf[32];
    unsigned int i = 0;

    if (write(STDOUT_FILENO, "\x1b[6n", 4) != 4) {
        return -1;
    }

    while (i < sizeof(buf) - 1) {
        if (read(STDIN_FILENO, &buf[i], 1) != 1) break;
        if (buf[i] == 'R') break;
        i++;
    }

    buf[i] = '\0';

    if (buf[0] != ESC_KEY || buf[1] != '[') {
        return -1;
    }

    if (sscanf(&buf[2], "%d;%d", rows, cols) != 2) {
        return -1;
    }

    return 0;
}

int getWindowSize(int *rows, int *cols) {
    struct winsize ws;

    if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0) {
        if (write(STDOUT_FILENO, "\x1b[999C\x1b[999B", 12) != 12) {
            return -1;
        }

        return getCursorPosition(rows, cols);
    } else {
        *cols = ws.ws_col;
        *rows = ws.ws_row;
        return 0;
    }
}

/*** syntax highlighting ***/

int is_separator(int c) {
    return isspace(c) || c == '\0' || strchr(",.()+-/*=~%<>[];", c) != NULL;
}

void editorUpdateSyntax(erow *row) {
    row->hl = realloc(row->hl, row->rsize);
    memset(row->hl, HL_NORMAL, row->rsize);

    if (E.syntax == NULL) {
        return;
    }

    char **keywords = E.syntax->keywords;

    char *scs = E.syntax->singleline_comment_start;
    char *mcs = E.syntax->multiline_comment_start;
    char *mce = E.syntax->multiline_comment_end;

    int scs_len = scs ? strlen(scs) : 0;
    int mcs_len = mcs ? strlen(mcs) : 0;
    int mce_len = mce ? strlen(mce) : 0;

    int prev_sep = 1;
    int in_comment = (row->idx > 0 && E.row[row->idx - 1].hl_open_comment);

    int i = 0;

    while (i < row->rsize) {
        char c = row->render[i];
        char next_c = (i + 1) < row->rsize ? row->render[i + 1] : 0;


        if (scs_len && !in_comment) {
            if (!strncmp(&row->render[i], scs, scs_len)) {
                memset(&row->hl[i], HL_COMMENT, row->rsize - i);
                break;
            }
        }

        if (mcs_len && mce_len) {
            if (in_comment) {
                row->hl[i] = HL_MLCOMMENT;
                if (!strncmp(&row->render[i], mce, mce_len)) {
                    memset(&row->hl[i], HL_MLCOMMENT, mce_len);
                    i += mce_len;
                    in_comment = 0;
                    prev_sep = 1;
                    continue;
                } else {
                    i++;
                    continue;
                }
            } else if (!strncmp(&row->render[i], mcs, mcs_len)) {
                memset(&row->hl[i], HL_MLCOMMENT, mcs_len);
                i += mcs_len;
                in_comment = 1;
                continue;
            }
        }

        if (E.syntax->flags & HL_HIGHLIGHT_STRINGS) {
            if (c == '"' || c == '\'') {
                int quote = c;
                row->hl[i++] = HL_STRING;
                c = row->render[i];

                while (i < row->rsize && c != quote) {
                    if (c == '\\' && i + 1 < row->rsize) {
                        row->hl[i++] = HL_STRING;
                        row->hl[i++] = HL_STRING;
                    } else {
                        row->hl[i++] = HL_STRING;
                    }

                    c = row->render[i];
                }

                if (c == quote) {
                    row->hl[i++] = HL_STRING;
                }

                prev_sep = 1;
                continue;
            }
        }

        if (E.syntax->flags & HL_HIGHLIGHT_NUMBERS && prev_sep) {
            if (c == '0' && (next_c == 'x' || next_c == 'X')) {
                row->hl[i++] = HL_NUMBER;
                row->hl[i++] = HL_NUMBER;
                c = row->render[i];

                while (i < row->rsize && ishexdigit(c)) {
                    row->hl[i++] = HL_NUMBER;
                    c = row->render[i];
                }

                prev_sep = 0;
                continue;
            }

            if (isdigit(c)) {
                while (i < row->rsize && isdigit(c)) {
                    row->hl[i++] = HL_NUMBER;
                    c = row->render[i];
                }

                prev_sep = 0;
                continue;
            }
        }

        if (prev_sep) {
            int j;
            for (j = 0; keywords[j]; j++) {
                int klen = strlen(keywords[j]);
                int kw2 = keywords[j][klen - 1] == '|';
                if (kw2) klen--;

                if (!strncmp(&row->render[i], keywords[j], klen) &&
                        is_separator(row->render[i + klen])
                   ) {
                    memset(&row->hl[i], kw2 ? HL_KEYWORD2 : HL_KEYWORD1, klen);
                    i += klen;
                    break;
                }
            }

            if (keywords[j] != NULL) {
                prev_sep = 0;
                continue;
            }
        }

        prev_sep = is_separator(c);
        i++;
    }

    int changed = (row->hl_open_comment != in_comment);
    row->hl_open_comment = in_comment;
    if (changed && row->idx + 1 < E.numrows) {
        editorUpdateSyntax(&E.row[row->idx + 1]);
    }
}

int editorSyntaxToColor(int hl) {
    switch (hl) {
        case HL_COMMENT:
        case HL_MLCOMMENT: return 36;
        case HL_KEYWORD1: return 33;
        case HL_KEYWORD2: return 32;
        case HL_STRING: return 35;
        case HL_NUMBER: return 31;
        case HL_MATCH: return 34;
        default: return 37;
    }
}

void editorSelectSyntaxHighlight() {
    E.syntax = NULL;
    if (E.filename == NULL) {
        return;
    }

    char *ext = strrchr(E.filename, '.');

    for (unsigned int j = 0; j < HLDB_ENTRIES; j++) {
        struct editorSyntax *s = &HLDB[j];
        unsigned int i = 0;

        while (s->filematch[i]) {
            int is_ext = (s->filematch[i][0] == '.');

            if ((is_ext && ext && !strcmp(ext, s->filematch[i])) ||
                    (!is_ext && strstr(E.filename, s->filematch[i])))
            {
                E.syntax = s;

                int filerow;
                for (filerow = 0; filerow < E.numrows; filerow++) {
                    editorUpdateSyntax(&E.row[filerow]);
                }

                return;
            }

            i++;
        }
    }
}

/*** row operations ***/
int getCharLen(const char *string, int pos) {
    return mblen(string + pos, strlen(string) - pos);
}

int getPrevCharLen(const char *string, int pos) {
    int len = 0;
    int i = pos;

    while (i > 0 && len < 1) {
        i--;
        len = mblen(string + i, pos);
    }

    return len;
}

int getCharWidth(char *string, int size, int *l) {
    wchar_t dest;
    int len = mblen(string, size);
    if (len <= 0) {
        *l = 1;
        return 1;
    }

    *l = len;
    mbtowc(&dest, string, size);

    return wcwidth(dest);
}

int editorRowCxToRx(erow *row, int cx) {
    int rx = 0;
    int j;
    int len;
    char *line = row->chars;

    for (j = 0; j < cx;) {
        if (IS_TAB(*line)) {
            len = 1;
            rx += KILO_TAB_STOP - (rx % KILO_TAB_STOP);
        } else {
            int width = getCharWidth(line, row->size - j, &len);
            rx += width;
        }

        j += len;
        line += len;
    }

    return rx;
}


int editorRowRxToCx(erow *row, int rx) {
    int cur_rx = 0;
    int cx;
    int len;
    for (cx = 0; cx < row->size; ) {
        if (IS_TAB(row->chars[cx])) {
            cur_rx += (KILO_TAB_STOP - 1) - (cur_rx % KILO_TAB_STOP);
            cur_rx++;
            len = 1;
        } else {
            int width = getCharWidth(row->chars + cx, row->size - cx, &len);
            cur_rx += width;
        }

        if (cur_rx > rx) {
            return cx;
        }

        cx += len;
    }

    return cx;
}

void editorUpdateRow(erow *row) {
    int tabs = 0;
    int j;

    for (j = 0; j < row->size; j++) {
        if (IS_TAB(row->chars[j])) tabs++;
    }

    free(row->render);
    row->render = malloc(row->size + tabs*(KILO_TAB_STOP - 1) + 1);

    int idx = 0;
    int col = 0;
    int char_len;

    for (j = 0; j < row->size;) {
        if (IS_TAB(row->chars[j])) {
            j++;
            col++;
            row->render[idx++] = ' ';
            while (col % KILO_TAB_STOP != 0) {
                col++;
                row->render[idx++] = ' ';
            }
        } else {
            col += getCharWidth(row->chars+j, row->size - j, &char_len);
            while (char_len > 0) {
                char_len--;
                row->render[idx++] = row->chars[j++];
            }
        }
    }

    row->render[idx] = '\0';
    row->rsize = idx;

    editorUpdateSyntax(row);
}

void editorInsertRow(int at, char *s, size_t len) {
    if (at < 0 || at > E.numrows) {
        return;
    }

    E.row = realloc(E.row, sizeof(erow) * (E.numrows + 1));
    memmove(&E.row[at + 1], &E.row[at], sizeof(erow) * (E.numrows - at));

    for (int j = at + 1; j <= E.numrows; j++) {
        E.row[j].idx++;
    }

    E.row[at].idx = at;

    E.row[at].size = len;
    E.row[at].chars = malloc(len + 1);
    memcpy(E.row[at].chars, s, len);
    E.row[at].chars[len] = '\0';

    E.row[at].rsize = 0;
    E.row[at].render = NULL;
    E.row[at].hl = NULL;
    E.row[at].hl_open_comment = 0;
    editorUpdateRow(&E.row[at]);

    E.numrows++;
    E.dirty++;
    E.leftpad = numDigits(E.numrows) + 1;
}

void editorFreeRow(erow *row) {
    free(row->render);
    free(row->chars);
    free(row->hl);
}

void editorDelRow(int at) {
    if (at < 0 || at >= E.numrows) {
        return;
    }

    editorFreeRow(&E.row[at]);
    memmove(&E.row[at], &E.row[at + 1], sizeof(erow) * (E.numrows - at - 1));

    for (int j = at; j < E.numrows - 1; j++) {
        E.row[j].idx--;
    }

    E.numrows--;
    E.dirty++;
    E.leftpad = numDigits(E.numrows) + 1;
}

void editorRowInsertChar(erow *row, int at, int c) {
    if (at < 0 || at > row->size) {
        at = row->size;
    }

    row->chars = realloc(row->chars, row->size + 2);
    memmove(&row->chars[at + 1], &row->chars[at], row->size - at + 1);
    row->size++;
    row->chars[at] = c;
    editorUpdateRow(row);
    E.dirty++;
}

void editorRowAppendString(erow *row, char *s, size_t len) {
    row->chars = realloc(row->chars, row->size + len + 1);
    memcpy(&row->chars[row->size], s, len);
    row->size += len;
    row->chars[row->size] = '\0';
    editorUpdateRow(row);
    E.dirty++;
}

int editorRowDelPrevChar(erow *row, int at) {
    if (at < 0 || at > row->size) {
        return 0;
    }

    int len = getPrevCharLen(row->chars, at);
    int pos = at - len;

    memmove(&row->chars[pos], &row->chars[at], row->size - pos);
    row->size -= len;
    editorUpdateRow(row);
    E.dirty++;

    return len;
}

/*** editor operations ***/
void editorInsertChar(int c) {
    if (E.cy == E.numrows) {
        editorInsertRow(E.numrows, "", 0);
    }

    editorRowInsertChar(&E.row[E.cy], E.cx, c);
    E.cx++;
}

void editorAutoIndent(int prev) {
    if (prev < 0) return;
    char *prev_row = E.row[prev].chars;
    char last_char = -1;

    while (IS_TAB(*prev_row) || *prev_row == ' ') {
        editorInsertChar(*prev_row);
        prev_row++;
    }

    while (*prev_row) {
        if (!IS_TAB(*prev_row) && *prev_row != ' ') {
            last_char = *prev_row;
        }
        prev_row++;
    }

    if (E.syntax && strchr(E.syntax->indent_chars, last_char) != NULL) {
        if (E.expandtab) {
            for (int i = 0; i < KILO_TAB_STOP; i++) {
                editorInsertChar(' ');
            }
        } else {
            editorInsertChar('\t');
        }
    }
}

void editorInsertNewline() {
    if (E.cx == 0) {
        editorInsertRow(E.cy, "", 0);
    } else {
        erow *row = &E.row[E.cy];
        editorInsertRow(E.cy + 1, &row->chars[E.cx], row->size - E.cx);
        row = &E.row[E.cy];
        row->size = E.cx;
        row->chars[row->size] = '\0';
        editorUpdateRow(row);
    }

    E.cy++;
    E.cx = 0;
}

void editorDelChar() {
    if (E.cy == E.numrows) {
        return;
    } else if (E.cx == 0 && E.cy == 0) {
        return;
    }

    erow *row = &E.row[E.cy];
    if (E.cx > 0) {
        E.cx -= editorRowDelPrevChar(row, E.cx);
    } else {
        E.cx = E.row[E.cy - 1].size;
        editorRowAppendString(&E.row[E.cy - 1], row->chars, row->size);
        editorDelRow(E.cy);
        E.cy--;
    }
}


/*** file i/o ***/
char *editorRowsToString(int *buflen) {
    int totlen = 0;
    int j;
    for (j = 0; j < E.numrows; j++) {
        totlen += E.row[j].size + 1;
    }

    *buflen = totlen;

    char *buf = malloc(totlen);
    char *p = buf;
    for (j = 0; j < E.numrows; j++) {
        memcpy(p, E.row[j].chars, E.row[j].size);
        p += E.row[j].size;
        *p = '\n';
        p++;
    }

    return buf;
}

void editorOpen(char *filename) {
    free(E.filename);
    E.filename = strdup(filename);

    editorSelectSyntaxHighlight();

    FILE *fp = fopen(filename, "r");

    if (!fp) {
        die("fopen");
    }

    char *line = NULL;
    size_t linecap = 0;
    ssize_t linelen;

    while ((linelen = getline(&line, &linecap, fp)) != -1) {
        while (linelen > 0 && (line[linelen - 1] == '\n' ||
                    line[linelen - 1] == '\r')) {
            linelen--;
        }

        editorInsertRow(E.numrows, line, linelen);
    }

    free(line);
    fclose(fp);
    E.dirty = 0;
}

void editorSave() {
    if (E.filename == NULL) {
        E.filename = editorPrompt("Save as: %s (ESC to cancel)", NULL);
        if (E.filename == NULL) {
            editorSetStatusMessage("Save aborted");
            return;
        }
        editorSelectSyntaxHighlight();
    }

    int len;
    char *buf = editorRowsToString(&len);

    int fd = open(E.filename, O_RDWR | O_CREAT, 0644);
    if (fd != -1) {
        if (ftruncate(fd, len) != -1) {
            if (write(fd, buf, len) == len) {
                close(fd);
                free(buf);
                E.dirty = 0;
                editorSetStatusMessage("%d bytes written to disk", len);
                return;
            }
        }
        close(fd);
    }

    free(buf);
    editorSetStatusMessage("Can't save! I/O error: %s", strerror(errno));
}

/*** find ***/

void editorFindCallback(char *query, int key) {
    static int last_match = -1;
    static int direction = 1;

    static int saved_hl_line;
    static char *saved_hl = NULL;

    if (saved_hl) {
        memcpy(E.row[saved_hl_line].hl, saved_hl, E.row[saved_hl_line].rsize);
        free(saved_hl);
        saved_hl = NULL;
    }

    if (key == '\r' || key == ESC_KEY) {
        last_match = -1;
        direction = 1;
        return;
    } else if (key == ARROW_RIGHT || key == ARROW_DOWN) {
        direction = 1;
    } else if (key == ARROW_LEFT || key == ARROW_UP) {
        direction = -1;
    } else {
        last_match = -1;
        direction = 1;
    }

    if (last_match == -1) direction = 1;

    int current = last_match;
    int i;

    for (i = 0; i < E.numrows; i++) {
        current += direction;

        if (current == -1) {
            current = E.numrows - 1;
        } else if (current == E.numrows) {
            current = 0;
        }

        erow *row = &E.row[current];
        char *match = strstr(row->render, query);
        if (match) {
            last_match = current;
            E.cy = current;
            E.cx = editorRowRxToCx(row, match - row->render);
            E.rowoff = E.numrows;

            saved_hl_line = current;
            saved_hl = malloc(row->rsize);
            memcpy(saved_hl, row->hl, row->rsize);
            memset(&row->hl[match - row->render], HL_MATCH, strlen(query));
            break;
        }
    }
}

void editorFind() {
    int saved_cx = E.cx;
    int saved_cy = E.cy;
    int saved_coloff = E.coloff;
    int saved_rowoff = E.rowoff;

    char *query = editorPrompt("Search: %s", editorFindCallback);

    if (query) {
        free(query);
    } else {
        E.cx = saved_cx;
        E.cy = saved_cy;
        E.coloff = saved_coloff;
        E.rowoff = saved_rowoff;
    }
}

/*** append buffer ***/
struct abuf {
    char *b;
    int len;
};

#define ABUF_INIT {NULL, 0}

void abAppend(struct abuf *ab, const char *s, int len) {
    char *new = realloc(ab->b, ab->len + len);

    if (new == NULL) {
        return;
    }

    memcpy(&new[ab->len], s, len);
    ab->b = new;
    ab->len += len;
}

void abFree(struct abuf *ab) {
    free(ab->b);
}

/*** output ***/
int numDigits(int num) {
    int digits = 0;
    while (num > 0) {
        ++digits;
        num = num / 10;
    }
    return digits;
}

void editorScroll() {
    E.rx = 0;
    if (E.cy < E.numrows) {
        E.rx = editorRowCxToRx(&E.row[E.cy], E.cx);
    }

    if (E.cy < E.rowoff) {
        E.rowoff = E.cy;
    }
    if (E.cy >= E.rowoff + E.screenrows) {
        E.rowoff = E.cy - E.screenrows + 1;
    }
    if (E.rx < E.coloff) {
        E.coloff = E.rx;
    }

    E.rx += E.leftpad;
    if (E.rx >= E.coloff + E.screencols) {
        E.coloff = E.rx - E.screencols + 1;
    }
}

void editorDrawRows(struct abuf *ab) {
    int y;
    for (y = 0; y < E.screenrows; y++) {
        int filerow = y + E.rowoff;
        if (filerow >= E.numrows) {
            if (E.numrows == 0 && y == E.screenrows / 3) {
                char welcome[80];

                int welcomelen = snprintf(
                    welcome, sizeof(welcome),
                    "Kilo editor -- version %s", KILO_VERSION
                );

                if (welcomelen > E.screencols) {
                    welcomelen = E.screencols;
                }

                int padding = (E.screencols - welcomelen) / 2;
                if (padding) {
                    abAppend(ab, "~", 1);
                    padding--;
                }

                while (padding--) {
                    abAppend(ab, " ", 1);
                }

                abAppend(ab, welcome, welcomelen);
            } else {
                if (y == 0) {
                    abAppend(ab, "\x1b[33m1\x1b[39m ", 12);
                } else {
                    abAppend(ab, "~", 1);
                }
            }
        } else {
            char lineno[E.leftpad + 11];

            int lineno_len = snprintf(
                lineno, sizeof(lineno), "\x1b[33m%*d\x1b[39m ",
                E.leftpad - 1, E.row[filerow].idx + 1
            );

            abAppend(ab, lineno, lineno_len);

            char *c = E.row[filerow].render;

            unsigned char *hl = E.row[filerow].hl;
            int current_color = -1;

            int size = E.row[filerow].rsize;
            int char_len;
            int col = 0;
            int offset = 0;

            for (;;) {
                col += getCharWidth(c, size - offset, &char_len);
                offset += char_len;

                if (col <= E.coloff) {
                    c += char_len;
                    hl += char_len;
                    continue;
                }

                if (col - E.coloff > E.screencols - E.leftpad || *c == '\0') {
                    break;
                }

                if (iscntrl(*c)) {
                    char sym = (*c <= 26) ? '@' + *c : '?';
                    abAppend(ab, "\x1b[7m", 4);
                    abAppend(ab, &sym, 1);
                    abAppend(ab, "\x1b[m", 3);
                    if (current_color != -1) {
                        char buf[16];
                        int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", current_color);
                        abAppend(ab, buf, clen);
                    }
                } else if (*hl == HL_NORMAL) {
                    if (current_color != -1) {
                        abAppend(ab, "\x1b[39m", 5);
                        current_color = -1;
                    }
                    abAppend(ab, c, char_len);
                } else {
                    int color = editorSyntaxToColor(*hl);
                    if (color != current_color) {
                        current_color = color;
                        char buf[16];
                        int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", color);
                        abAppend(ab, buf, clen);
                    }
                    abAppend(ab, c, char_len);
                }

                c += char_len;
                hl += char_len;
            }
            abAppend(ab, "\x1b[39m", 5);
        }

        abAppend(ab, "\x1b[K", 3);
        abAppend(ab, "\r\n", 2);
    }
}

void editorDrawStatusBar(struct abuf *ab) {
    abAppend(ab, "\x1b[7m", 4);

    char status[80], rstatus[80], debugMsg[80];

    erow *row = (E.cy >= E.numrows) ? NULL : &E.row[E.cy];
    int charLen = row && E.cx < row->size ? getCharLen(row->chars, E.cx) : 0;

    if (charLen) {
        char currentChar[charLen];

        memcpy(currentChar, &row->chars[E.cx], charLen);
        currentChar[charLen] = 0;

        if (charLen == 1) {
            snprintf(debugMsg, sizeof(debugMsg), "[%d] \"%s\" [%d]", E.cx, currentChar, row->chars[E.cx]);
        } else if (charLen == 2) {
            snprintf(debugMsg, sizeof(debugMsg), "[%d] \"%s\" [%d, %d]", E.cx, currentChar, row->chars[E.cx], row->chars[E.cx + 1]);
        } else if (charLen == 3) {
            snprintf(debugMsg, sizeof(debugMsg), "[%d] \"%s\" [%d, %d, %d]", E.cx, currentChar, row->chars[E.cx], row->chars[E.cx + 1], row->chars[E.cx + 2]);
        }
    } else {
        debugMsg[0] = 0;
    }

    int len = snprintf(
        status, sizeof(status), "%.20s - %d lines %s | debug %s",
        E.filename ? E.filename : "[No Name]",
        E.numrows,
        E.dirty ? "(modified)" : "",
        debugMsg
    );

    int rlen = snprintf(
        rstatus, sizeof(rstatus), "%s | %d/%d",
        E.syntax ? E.syntax->filetype : "no ft",
        E.cy + 1,
        E.numrows
    );

    if (len > E.screencols) {
        len = E.screencols;
    }

    abAppend(ab, status, len);

    while (len < E.screencols) {
        if (E.screencols - len == rlen) {
            abAppend(ab, rstatus, rlen);
            break;
        } else {
            abAppend(ab, " ", 1);
            len++;
        }
    }

    abAppend(ab, "\x1b[m", 3);
    abAppend(ab, "\r\n", 2);
}

void editorDrawMessageBar(struct abuf *ab) {
    abAppend(ab, "\x1b[K", 3);
    int msglen = strlen(E.statusmsg);

    if (msglen > E.screencols) {
        msglen = E.screencols;
    }

    if (msglen && time(NULL) - E.statusmsg_time < 5) {
        abAppend(ab, E.statusmsg, msglen);
    }
}

void editorRefreshScreen() {
    editorScroll();

    struct abuf ab = ABUF_INIT;

    abAppend(&ab, "\x1b[?25l", 6);
    abAppend(&ab, "\x1b[H", 3);

    editorDrawRows(&ab);
    editorDrawStatusBar(&ab);
    editorDrawMessageBar(&ab);

    char buf[32];
    snprintf(
        buf, sizeof(buf), "\x1b[%d;%dH", (E.cy - E.rowoff) + 1,
        (E.rx - E.coloff) + 1
    );

    abAppend(&ab, buf, strlen(buf));
    abAppend(&ab, "\x1b[?25h", 6);

    write(STDOUT_FILENO, ab.b, ab.len);
    abFree(&ab);
}

void editorSetStatusMessage(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(E.statusmsg, sizeof(E.statusmsg), fmt, ap);
    va_end(ap);
    E.statusmsg_time = time(NULL);
}

/*** input ***/

char *editorPrompt(char *prompt, void (*callback)(char *, int)) {
    size_t bufsize = 128;
    char *buf = malloc(bufsize);
    char cursor[20];
    int cursor_start = strstr(prompt, "%s") - prompt;

    size_t buflen = 0;
    buf[0] = '\0';

    while (1) {
        editorSetStatusMessage(prompt, buf);
        editorRefreshScreen();

        // move cursor to prompt
        int cursor_pos = cursor_start + buflen + 1;
        int len = snprintf(
            cursor, sizeof(cursor), "\x1b[%d;%dH",
            E.screenrows + 2, cursor_pos
        );

        write(STDOUT_FILENO, cursor, len);

        int c = editorReadKey();
        if (c == DEL_KEY || c == CTRL_KEY('h') || c == BACKSPACE) {
            if (buflen != 0) {
                buf[--buflen] = '\0';
            }
        } else if (c == ESC_KEY) {
            editorSetStatusMessage("");
            if (callback) {
                callback(buf, c);
            }
            free(buf);
            return NULL;
        } else if (c == '\r') {
            if (buflen != 0) {
                editorSetStatusMessage("");
                if (callback) {
                    callback(buf, c);
                }
                return buf;
            }
        } else if (!iscntrl(c) && c < 128) {
            if (buflen == bufsize - 1) {
                bufsize *= 2;
                buf = realloc(buf, bufsize);
            }
            buf[buflen++] = c;
            buf[buflen] = '\0';
        }

        if (callback) callback(buf, c);
    }
}

void editorMoveCursor(int key) {
    erow *row = (E.cy >= E.numrows) ? NULL : &E.row[E.cy];

    switch (key) {
        case ARROW_LEFT:
            if (E.cx > 0) {
                E.cx -= getPrevCharLen(row->chars, E.cx);
            } else if (E.cy > 0) {
                E.cy--;
                E.cx = E.row[E.cy].size;
            }
        break;

        case ARROW_RIGHT:
            if (row && E.cx < row->size) {
                E.cx += getCharLen(row->chars, E.cx);

                if (E.mode == NORMAL_MODE && E.cx >= row->size && E.cy < E.numrows - 1) {
                    E.cy++;
                    E.cx = 0;
                }
            } else if (row && E.cx == row->size && E.cy < E.numrows - 1) {
                E.cy++;
                E.cx = 0;
            }
        break;

        case ARROW_UP:
            if (E.cy > 0) {
                E.cy--;
                erow *newrow = E.cy >= E.numrows ? NULL : &E.row[E.cy];
                if (newrow) {
                    E.cx = editorRowRxToCx(newrow, E.rx - E.leftpad);
                }
            }
        break;

        case ARROW_DOWN:
            if (E.cy < E.numrows - 1) {
                E.cy++;
                erow *newrow = E.cy >= E.numrows ? NULL : &E.row[E.cy];
                if (newrow) {
                    E.cx = editorRowRxToCx(newrow, E.rx - E.leftpad);
                }
            }
        break;

        case HOME_KEY:
            E.cx = 0;
        break;

        case END_KEY:
            if (E.cy < E.numrows) {
                E.cx = E.row[E.cy].size;
            }
        break;

        case PAGE_UP:
        case PAGE_DOWN:
            {
                if (key == PAGE_UP) {
                    E.cy = E.rowoff;
                } else if (key == PAGE_DOWN) {
                    E.cy = E.rowoff + E.screenrows - 1;
                    if (E.cy > E.numrows) E.cy = E.numrows;
                }

                int times = E.screenrows;
                while (times--) {
                    editorMoveCursor(key == PAGE_UP ? ARROW_UP : ARROW_DOWN);
                }
            }
        break;
    }

    if (E.mode == INSERT_MODE) return;

    row = (E.cy >= E.numrows) ? NULL : &E.row[E.cy];

    if (row && E.cx >= row->size) {
        E.cx = row->size - getPrevCharLen(row->chars, E.cx);
    }
}

void editorProcessInsertModeKeypress() {
    static int quit_times = KILO_QUIT_TIMES;

    int c = editorReadKey();

    switch (c) {
        case '\r':
            editorInsertNewline();
            editorAutoIndent(E.cy - 1);
        break;

        case CTRL_KEY('q'):
            if (E.dirty && quit_times > 0) {
                editorSetStatusMessage("WARNING!!! File has unsaved changes. "
                        "Press Ctrl-Q %d more times to quit.", quit_times);
                quit_times--;
                return;
            }
            write(STDOUT_FILENO, "\x1b[2J", 4);
            write(STDOUT_FILENO, "\x1b[H", 3);
            exit(0);
        break;

        case CTRL_KEY('s'):
            editorSave();
        break;

        case CTRL_KEY('f'):
            editorFind();
        break;

        case BACKSPACE:
        case CTRL_KEY('h'):
        case DEL_KEY:
            if (c == DEL_KEY) {
                if (E.cy >= E.numrows) {
                    return;
                }

                if (E.cy < E.numrows - 1 || E.cx < E.row[E.cy].size) {
                    editorMoveCursor(ARROW_RIGHT);
                } else {
                    return;
                }
            }

            editorDelChar();
        break;

        case ARROW_UP:
        case ARROW_DOWN:
        case ARROW_LEFT:
        case ARROW_RIGHT:
        case HOME_KEY:
        case END_KEY:
        case PAGE_UP:
        case PAGE_DOWN:
            editorMoveCursor(c);
        break;

        case CTRL_KEY('l'):
        break;

        case ESC_KEY:
            E.mode = NORMAL_MODE;
            if (E.cx > 0) {
                editorMoveCursor(ARROW_LEFT);
            }
        break;

        default:
            if (!IS_TAB(c) || E.expandtab == 0) {
                if (!E.syntax || strchr(E.syntax->unindent_chars, c) == NULL) {
                    editorInsertChar(c);
                    return;
                }


                for (int i = E.cx - 1; i >= 0; i--) {
                    char p = E.row[E.cy].chars[i];
                    if (!IS_TAB(p) && p != ' ') {
                        editorInsertChar(c);
                        return;
                    }
                }

                for (int i = 0; i < KILO_TAB_STOP; i++) {
                    if (E.cx > 0) editorDelChar();
                }

                editorInsertChar(c);
                return;
            }

            int tabs = KILO_TAB_STOP - (E.rx - E.leftpad) % KILO_TAB_STOP;
            for (int i = 0; i < tabs; i++) {
                editorInsertChar(' ');
            }
        break;
    }

    quit_times = KILO_QUIT_TIMES;
}

void editorProcessNormalModeKeypress() {
    static int g_key_counter = 0;
    static int count = 0;

    int c = editorReadKey();

    if (isdigit(c) && (count || (!count && c != '0'))) {
        count = count * 10 + (c - '0');
        g_key_counter = 0;
        return;
    }

    int repeat = count > 0 ? count : 1;

    switch (c) {
        case 'h':
        case ARROW_LEFT:
            for (int i = 0; i < repeat; i++) {
                editorMoveCursor(ARROW_LEFT);
            }
        break;

        case 'l':
        case ARROW_RIGHT:
            for (int i = 0; i < repeat; i++) {
                editorMoveCursor(ARROW_RIGHT);
            }
        break;

        case 'j':
        case ARROW_DOWN:
            for (int i = 0; i < repeat; i++) {
                editorMoveCursor(ARROW_DOWN);
            }
        break;

        case 'k':
        case ARROW_UP:
            for (int i = 0; i < repeat; i++) {
                editorMoveCursor(ARROW_UP);
            }
        break;

        case '0':
        case HOME_KEY:
            editorMoveCursor(HOME_KEY);
        break;

        case '$':
        case END_KEY:
            editorMoveCursor(END_KEY);
        break;

        case PAGE_UP:
        case PAGE_DOWN:
            editorMoveCursor(c);
        break;

        case 'G':
            if (count == 0) {
                E.cy = E.numrows ? E.numrows - 1 : 0;
                E.cx = 0;
            } else {
                E.cy = 0;
                E.cx = 0;
                for (int i = 0; i < repeat - 1; i++) {
                    editorMoveCursor(ARROW_DOWN);
                }
            }
        break;

        case 'g':
            g_key_counter++;
            if (g_key_counter == 2) {
                E.cy = 0;
                E.cx = 0;
                for (int i = 0; i < repeat - 1; i++) {
                    editorMoveCursor(ARROW_DOWN);
                }
            } else {
                return;
            }
        break;

        case 'I':
            editorMoveCursor(HOME_KEY);

            if (E.cy < E.numrows) {
                erow *row = &E.row[E.cy]; 
                char *line = row->chars;

                while (isspace(*line)) {
                    line++;
                    editorMoveCursor(ARROW_RIGHT);
                }
            }

            E.mode = INSERT_MODE;
        break;

        case 'i':
            E.mode = INSERT_MODE;
        break;

        case 'A':
            E.mode = INSERT_MODE;
            editorMoveCursor(END_KEY);
        break;

        case 'a':
            E.mode = INSERT_MODE;
            editorMoveCursor(ARROW_RIGHT);
        break;

        case 'o':
            E.mode = INSERT_MODE;
            editorMoveCursor(END_KEY);
            editorInsertNewline();
            editorAutoIndent(E.cy - 1);
        break;

        case 'O':
            E.mode = INSERT_MODE;
            editorMoveCursor(HOME_KEY);
            editorInsertNewline();
            editorMoveCursor(ARROW_UP);
            editorAutoIndent(E.cy - 1);
        break;
    }

    count = 0;
    g_key_counter = 0;
}

void editorProcessKeypress() {
    if (E.mode == NORMAL_MODE) {
        editorProcessNormalModeKeypress();
    } else {
        editorProcessInsertModeKeypress();
    }
}

/*** init ***/

void initEditor() {
    E.cx = 0;
    E.cy = 0;
    E.rx = 0;
    E.rowoff = 0;
    E.coloff = 0;
    E.leftpad = 2;
    E.numrows = 0;
    E.row = NULL;
    E.dirty = 0;
    E.filename = NULL;
    E.statusmsg[0] = '\0';
    E.statusmsg_time = 0;
    E.syntax = NULL;
    E.mode = NORMAL_MODE;
    E.expandtab = 1;

    write(STDOUT_FILENO, "\033[?47h", 6);
    saveCursorPosition();

    if (getWindowSize(&E.screenrows, &E.screencols) == -1) {
        die("getWindowSize");
    }

    E.screenrows -= 2;
}

int main(int argc, char *argv[]) {
    setlocale(LC_ALL, "");
    enableRawMode();
    initEditor();

    if (argc >= 2) {
        editorOpen(argv[1]);
    }

    editorSetStatusMessage(
        "HELP: Ctrl-S = save | Ctrl-Q = quit | Ctrl-F = find"
    );

    while (1) {
        editorRefreshScreen();
        editorProcessKeypress();
    }

    return 0;
}

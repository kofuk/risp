#include <assert.h>
#include <ctype.h>
#include <signal.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <readline/history.h>
#include <readline/readline.h>

#include "parse.h"
#include "risp.h"
#include "rt.h"

static int file_getc(lexer *lex) { return fgetc(lex->infile); }

static int file_ungetc(int c, lexer *lex) { return ungetc(c, lex->infile); }

static int readline_getc(lexer *lex) {
    if (lex->rl_unget >= 0) {
        int c = lex->rl_unget;
        lex->rl_unget = -1;
        return c;
    }

    if (lex->rl_line == NULL || lex->rl_line[lex->rl_cursor] == '\0') {
        if (!lex->rl_nul_read) {
            lex->rl_nul_read = true;
            return '\n';
        }

        free(lex->rl_line);
        lex->rl_line = readline(lex->rl_prompt);
        add_history(lex->rl_line);
        lex->rl_cursor = 0;
        lex->rl_nul_read = false;
    }
    if (lex->rl_line == NULL) {
        return EOF;
    } else if (lex->rl_line[lex->rl_cursor] == '\0') {
        lex->rl_nul_read = true;
        return '\n';
    }

    int c = lex->rl_line[lex->rl_cursor];
    lex->rl_cursor++;
    return c;
}

static int readline_ungetc(int c, lexer *lex) {
    lex->rl_unget = c;
    return c;
}

int main(int argc, char **argv) {
    lexer lex = {
        .rl_prompt = ">>> ",
        .rl_line = NULL,
        .rl_cursor = 0,
        .rl_unget = -1,
        .rl_nul_read = true,
        .repl = false,
    };

    if (argc < 2) {
        lex.in_name = "<stdin>";
        if (isatty(STDIN_FILENO)) {
            lex.getc = &readline_getc;
            lex.ungetc = &readline_ungetc;
            lex.repl = true;
        } else {
            lex.infile = stdin;
            lex.getc = &file_getc;
            lex.ungetc = &file_ungetc;
        }
    } else {
        FILE *infile = fopen(argv[1], "r");
        if (infile == NULL) {
            perror("open: cannot open input file");
            return 1;
        }

        lex.in_name = argv[1];
        lex.infile = infile;
        lex.getc = &file_getc;
        lex.ungetc = &file_ungetc;
    }

    lex_state state;
    lex_state_init(&state);
    lex.state = &state;
    lex.tk = NULL;
    risp_env env;
    env_init(&env);
    init_native_functions(&env);

    int exit_code = 0;

    for (;;) {
        i32 status = read_and_eval(&lex, &env);

        if (status <= 0) {
            exit_code = -status;
            goto clean;
        }
    }

clean:
    token_free(lex.tk);
    var_frame_free_all(&env);
    free(env.heap);

    if (lex.infile != NULL) {
        fclose(lex.infile);
    }

    return exit_code;
}

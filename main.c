#include <assert.h>
#include <ctype.h>
#include <signal.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#ifdef HAVE_READLINE
#include <readline/history.h>
#include <readline/readline.h>
#endif

#include "parse.h"
#include "risp.h"
#include "rt.h"

static int file_read_char(lexer *lex) { return fgetc(lex->infile); }

static int file_unread_char(int c, lexer *lex) { return ungetc(c, lex->infile); }

#ifdef HAVE_READLINE
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
#endif

int main(int argc, char **argv) {
    lexer lex = {
#if HAVE_READLINE
        .rl_prompt = ">>> ",
        .rl_line = NULL,
        .rl_cursor = 0,
        .rl_unget = -1,
        .rl_nul_read = true,
#endif
        .repl = false,
    };

    if (argc < 2) {
        lex.in_name = "<stdin>";
#ifdef HAVE_READLINE
        if (isatty(STDIN_FILENO)) {
            lex.getc = &readline_getc;
            lex.ungetc = &readline_ungetc;
            lex.repl = true;
        } else {
#endif
            lex.infile = stdin;
            lex.read_char = &file_read_char;
            lex.unread_char = &file_unread_char;
#ifdef HAVE_READLINE
        }
#endif

        argc = 1;
        argv[0] = "<stdin>";
    } else {
        FILE *infile = fopen(argv[1], "r");
        if (infile == NULL) {
            perror("open: cannot open input file");
            return 1;
        }

        lex.in_name = argv[1];
        lex.infile = infile;
        lex.read_char = &file_read_char;
        lex.unread_char = &file_unread_char;

        --argc;
        ++argv;
    }

    int exit_code = 0;

    lex_state state;
    lex_state_init(&state);
    lex.state = &state;
    lex.tk = NULL;
    risp_env env;
    env_init(&env, argc, argv);
    init_native_functions(&env);
    if (!init_std_module(&env)) {
        fputs("Failed to load std module.\n", stderr);
        goto clean;
    }

    for (;;) {
        i32 status = read_and_eval(&lex, &env, true);

        if (status <= 0) {
            exit_code = -status;
            goto clean;
        }
    }

clean:
#ifdef HAVE_READLINE
    free(lex.rl_line);
#endif

    token_free(lex.tk);
    var_frame_free_all(&env);
    free(env.heap);

    if (lex.infile != NULL) {
        fclose(lex.infile);
    }

    return exit_code;
}

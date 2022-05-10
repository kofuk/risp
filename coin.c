#include <assert.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include <sys/cdefs.h>
#include <unistd.h>
#include <stdint.h>
#include <time.h>

#include "rt.h"

typedef uint32_t position;

typedef enum {
    D_UP, D_RIGHT, D_DOWN, D_LEFT
} direction;

typedef struct operation {
    size_t prev_index;
    position pos;
} operation;

typedef struct {
    size_t len;
    size_t cap;
    size_t cur_head;
    operation *ops;
    char *visit_states;
} op_queue;

static inline void queue_maybe_extend(op_queue *queue) {
    if (queue->len + 14 >= queue->cap) {
        if (queue->cap == 0) {
            queue->cap = 64;
        } else {
            queue->cap <<= 1;
        }
        queue->ops = realloc(queue->ops, sizeof(operation) * queue->cap);
    }
}

static position init_pos(risp_env *env, char *w, char *b) {
    size_t w_len = strlen(w);
    size_t b_len = strlen(b);
    size_t longer = w_len < b_len ? b_len : w_len;
    // Set some bits to 1 so that I can use index of 14 as sentinel value.
    position pos = 0x40004000;
    for (unsigned int i = 0; i < longer; ++i) {
        if (i < w_len) {
            int w_index = w[i] - 'A';
            if (((pos >> w_index) & 1) != 0) {
                signal_error_s(env, "Coins are overlapping");
                return 0;
            }
            pos |= 1 << w_index;
        }

        if (i < b_len) {
            int b_index = (b[i] - 'A') | 0x10;
            if (((pos >> b_index) & 1) != 0) {
                signal_error_s(env, "Coins are overlapping");
                return 0;
            }
            pos |= 1 << b_index;
        }
    }
    return pos;
}

static const unsigned int neighbor[14] = {
    // Use 0xE (14) for sentinel.
    0xEE1E, // A
    0x0E2E, // B
    0x13BE, // C
    0xE4E2, // D
    0xE5E3, // E
    0xE6E4, // F
    0xEEE5, // G
    0xE8EE, // H
    0xE9E7, // I
    0xEAE8, // J
    0xEBE9, // K
    0x2ECA, // L
    0xBEDE, // M
    0xCEEE  // N
};

static inline bool can_move(position pos, int where, direction dir) {
    int to = (neighbor[where] >> (dir + dir + dir + dir)) & 0xF;
    return !((pos >> to) & 1) && !((pos >> (to | 0x10)) & 1);
}

static position move(position pos, int where, direction dir) {
    position result = pos;

    int index = where;
    if (!((pos >> index) & 1)) {
        index |= 0x10;
    }

    int to = (neighbor[where] >> (dir + dir + dir + dir)) & 0xF;
    assert(to >= 0);
    assert(!((pos & (1 << to)) & 1));
    assert(!((pos & (1 << (to | 0x10))) & 1));

    to |= index & 0x10;
    result |= 1 << to;
    result &= ~(1 << index);

    return result;
}

static inline unsigned int pos_to_state_index(position pos) {
    return (pos & 0x3FFF) | ((pos & 0x3FFF0000) >> 2);
}

static inline bool is_visited(op_queue *queue, position moved_pos) {
    return (bool)queue->visit_states[pos_to_state_index(moved_pos)];
}

static inline void mark_visited(op_queue *queue, position pos) {
    queue->visit_states[pos_to_state_index(pos)] = 1;
}

static void search_next_state(op_queue *queue) {
    position cur_pos = queue->ops[queue->cur_head].pos;
    queue_maybe_extend(queue);

    for (int i = 0; i < 14; ++i) {
        if (cur_pos & (0x10001 << i)) {
#define push_state(direction)                                           \
            do {                                                        \
                if (can_move(cur_pos, i, (direction))) {                \
                    position moved_pos = move(cur_pos, i, (direction)); \
                    if (!is_visited(queue, moved_pos)) {                \
                        operation *op = &queue->ops[queue->len];        \
                        op->pos = moved_pos;                            \
                        op->prev_index = queue->cur_head;               \
                        queue->len++;                                   \
                        mark_visited(queue, op->pos);                   \
                    }                                                   \
                }                                                       \
            } while (0)

            push_state(D_UP);
            push_state(D_RIGHT);
            push_state(D_DOWN);
            push_state(D_LEFT);

#undef push_state
        }
    }
}

static inline void print_item(position pos, int where) {
    if ((pos >> where) & 1) {
        putchar('w');
    } else if ((pos >> (where | 0x10)) & 1) {
        putchar('b');
    } else {
        putchar(' ');
    }
}

static __attribute__((unused)) void print_pos(position pos) {
    fputs("   ", stdout);
    print_item(pos, 'H' - 'A');
    fputs("\n   ", stdout);
    print_item(pos, 'I' - 'A');
    fputs("\n   ", stdout);
    print_item(pos, 'J' - 'A');
    fputs("\n   ", stdout);
    print_item(pos, 'K' - 'A');
    puts("");
    print_item(pos, 'A' - 'A');
    print_item(pos, 'B' - 'A');
    print_item(pos, 'C' - 'A');
    print_item(pos, 'L' - 'A');
    print_item(pos, 'M' - 'A');
    print_item(pos, 'N' - 'A');
    fputs("\n  ", stdout);
    print_item(pos, 'D' - 'A');
    fputs("\n  ", stdout);
    print_item(pos, 'E' - 'A');
    fputs("\n  ", stdout);
    print_item(pos, 'F' - 'A');
    fputs("\n  ", stdout);
    print_item(pos, 'G' - 'A');
    puts("");
}

static void get_move(position from, position to, char *move) {
    char from_pos = 0;
    char to_pos = 0;
    for (int i = 0; i < 14; ++i) {
        if (((from >> i) & 1) != ((to >> i) & 1)) {
            if (((from >> i) & 1) == 0) {
                assert(to_pos == 0);
                to_pos = (char)i + 'A';
            } else {
                assert(from_pos == 0);
                from_pos = (char)i + 'A';
            }
        } else if (((from >> (i | 0x10)) & 1) != ((to >> (i | 0x10)) & 1)) {
            if (((from >> (i | 0x10)) & 1) == 0) {
                assert(to_pos == 0);
                to_pos = (char)i + 'A';
            } else {
                assert(from_pos == 0);
                from_pos = (char)i + 'A';
            }
        }

    }
    move[0] = from_pos;
    move[1] = to_pos;
}

static char *get_solution(const op_queue *queue) {
    size_t len = 0;
    size_t cap = 64;
    char *move = malloc(cap);
    size_t current = queue->cur_head;
    while (current != 0) {
        if (len + 1 >= cap) {
            cap <<= 1;
            move = realloc(move, cap);
        }

        size_t prev = queue->ops[current].prev_index;
        get_move(queue->ops[prev].pos, queue->ops[current].pos, move + len);
        len += 2;

        current = queue->ops[current].prev_index;
    }

    if (len >= cap) {
        cap <<= 1;
        move = realloc(move, cap);
    }
    move[len] = '\0';

    return move;
}

char *do_coin(risp_env *env, char *w1, char *b1, char *w2, char *b2) {
    if (strlen(w1) != strlen(w2) || strlen(b1) != strlen(b2)) {
        signal_error_s(env, "The number of conis is different between the initial and final positions");
        return NULL;
    }

    position last = init_pos(env, w1, b1);
    if (get_error(env) != &Qnil) {
        return NULL;
    }
    op_queue queue = {0};

    queue.visit_states = malloc(sizeof(char) * 268435456);
    memset(queue.visit_states, 0, sizeof(char) * 268435456);

    queue_maybe_extend(&queue);
    queue.len++;
    queue.ops[0].prev_index = 0;
    queue.ops[0].pos = init_pos(env, w2, b2);
    if (get_error(env) != &Qnil) {
        return NULL;
    }

    bool solved = false;
    while (queue.cur_head < queue.len) {
        if (queue.ops[queue.cur_head].pos == last) {
            solved = true;
            break;
        }

        search_next_state(&queue);
        queue.cur_head++;
    }

    char *result = NULL;
    if (solved) {
        result = get_solution(&queue);
    } else {
        signal_error_s(env, "No solution");
    }

    free(queue.ops);
    free(queue.visit_states);

    return result;
}

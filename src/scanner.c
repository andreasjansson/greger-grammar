#include "tree_sitter/parser.h"
#include <wctype.h>
#include <string.h>

enum TokenType {
    TOOL_CONTENT,
    HTML_COMMENT,
};

typedef struct {
    char tool_id[256];
    bool in_tool_content;
} Scanner;

static inline void advance(TSLexer *lexer) { lexer->advance(lexer, false); }

static inline void skip(TSLexer *lexer) { lexer->advance(lexer, true); }

unsigned tree_sitter_greger_external_scanner_serialize(void *payload, char *buffer) {
    Scanner *scanner = (Scanner *)payload;

    size_t tool_id_len = strlen(scanner->tool_id);
    if (tool_id_len >= 255) tool_id_len = 255;

    buffer[0] = scanner->in_tool_content ? 1 : 0;
    buffer[1] = tool_id_len;
    memcpy(buffer + 2, scanner->tool_id, tool_id_len);

    return 2 + tool_id_len;
}

void tree_sitter_greger_external_scanner_deserialize(void *payload, const char *buffer, unsigned length) {
    Scanner *scanner = (Scanner *)payload;

    if (length == 0) {
        scanner->in_tool_content = false;
        scanner->tool_id[0] = '\0';
        return;
    }

    scanner->in_tool_content = buffer[0] == 1;
    if (length > 1) {
        unsigned tool_id_len = buffer[1];
        if (tool_id_len >= 255) tool_id_len = 255;
        if (length >= 2 + tool_id_len) {
            memcpy(scanner->tool_id, buffer + 2, tool_id_len);
            scanner->tool_id[tool_id_len] = '\0';
        } else {
            scanner->tool_id[0] = '\0';
        }
    } else {
        scanner->tool_id[0] = '\0';
    }
}

void *tree_sitter_greger_external_scanner_create(void) {
    Scanner *scanner = malloc(sizeof(Scanner));
    scanner->in_tool_content = false;
    scanner->tool_id[0] = '\0';
    return scanner;
}

void tree_sitter_greger_external_scanner_destroy(void *payload) {
    free(payload);
}

static bool scan_html_comment(TSLexer *lexer) {
    if (lexer->lookahead != '<') return false;
    advance(lexer);

    if (lexer->lookahead != '!') return false;
    advance(lexer);

    if (lexer->lookahead != '-') return false;
    advance(lexer);

    if (lexer->lookahead != '-') return false;
    advance(lexer);

    // Now we're inside the comment, scan until we find -->
    int dashes = 0;
    while (lexer->lookahead != 0) {
        if (lexer->lookahead == '-') {
            dashes++;
            advance(lexer);
        } else if (lexer->lookahead == '>' && dashes >= 2) {
            advance(lexer);
            lexer->result_symbol = HTML_COMMENT;
            return true;
        } else {
            dashes = 0;
            advance(lexer);
        }
    }

    return false;
}

static bool scan_tool_content(Scanner *scanner, TSLexer *lexer) {
    // Just consume one character and return true
    advance(lexer);
    return true;
}

bool tree_sitter_greger_external_scanner_scan(void *payload, TSLexer *lexer, const bool *valid_symbols) {
    Scanner *scanner = (Scanner *)payload;

    // Skip whitespace
    while (iswspace(lexer->lookahead) && lexer->lookahead != '\n') {
        skip(lexer);
    }

    // Debug: always try tool content if we see '<'
    if (lexer->lookahead == '<' && scan_tool_content(scanner, lexer)) {
        lexer->result_symbol = TOOL_CONTENT;
        return true;
    }

    if (valid_symbols[HTML_COMMENT] && scan_html_comment(lexer)) {
        return true;
    }

    return false;
}

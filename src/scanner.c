#include "tree_sitter/parser.h"
#include <wctype.h>
#include <string.h>
#include <stdio.h>

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
    if (lexer->lookahead != '<') return false;

    // Save position to backtrack if this isn't tool content
    int32_t start_pos = lexer->get_column(lexer);
    advance(lexer);

    // Check for "tool." - if not, backtrack
    if (lexer->lookahead != 't') {
        // This is not a tool content tag, backtrack and return false
        while (lexer->get_column(lexer) > start_pos) {
            // Can't actually backtrack in tree-sitter, so this won't work
        }
        return false;
    }
    advance(lexer);
    if (lexer->lookahead != 'o') return false;
    advance(lexer);
    if (lexer->lookahead != 'o') return false;
    advance(lexer);
    if (lexer->lookahead != 'l') return false;
    advance(lexer);
    if (lexer->lookahead != '.') return false;
    advance(lexer);

    // Get the tool ID
    char tool_id[256];
    int id_len = 0;
    while (lexer->lookahead != '>' && lexer->lookahead != 0 && id_len < 255) {
        tool_id[id_len++] = lexer->lookahead;
        advance(lexer);
    }
    tool_id[id_len] = '\0';

    if (lexer->lookahead != '>') return false;
    advance(lexer);

    // Now scan until we find the closing tag
    while (lexer->lookahead != 0) {
        if (lexer->lookahead == '<') {
            // Save position in case this isn't the closing tag
            advance(lexer);

            if (lexer->lookahead == '/') {
                advance(lexer);

                // Check "tool."
                if (lexer->lookahead == 't') {
                    advance(lexer);
                    if (lexer->lookahead == 'o') {
                        advance(lexer);
                        if (lexer->lookahead == 'o') {
                            advance(lexer);
                            if (lexer->lookahead == 'l') {
                                advance(lexer);
                                if (lexer->lookahead == '.') {
                                    advance(lexer);

                                    // Check if ID matches
                                    bool matches = true;
                                    for (int i = 0; i < id_len && matches; i++) {
                                        if (lexer->lookahead != tool_id[i]) {
                                            matches = false;
                                        } else {
                                            advance(lexer);
                                        }
                                    }

                                    if (matches && lexer->lookahead == '>') {
                                        // Found the exact closing tag, consume it
                                        advance(lexer);
                                        lexer->result_symbol = TOOL_CONTENT;
                                        return true;
                                    }
                                }
                            }
                        }
                    }
                }
            }
            // Continue if not the right closing tag - note we already advanced past '<'
        } else {
            advance(lexer);
        }
    }

    // Reached end without finding closing tag
    return false;
}

bool tree_sitter_greger_external_scanner_scan(void *payload, TSLexer *lexer, const bool *valid_symbols) {
    Scanner *scanner = (Scanner *)payload;

    // Skip whitespace but preserve newlines
    while (iswspace(lexer->lookahead) && lexer->lookahead != '\n') {
        skip(lexer);
    }

    // When we see <, try HTML comments first (more general), then tool content (more specific)
    if (lexer->lookahead == '<') {
        // Try HTML comment first if it's valid
        if (valid_symbols[HTML_COMMENT] && scan_html_comment(lexer)) {
            return true;
        }

        // Try tool content if it's valid
        if (valid_symbols[TOOL_CONTENT] && scan_tool_content(scanner, lexer)) {
            return true;
        }
    }

    return false;
}

#include "tree_sitter/parser.h"
#include <wctype.h>
#include <string.h>
#include <stdio.h>

enum TokenType {
    TOOL_START_TAG,
    TOOL_END_TAG,
    TOOL_CONTENT_HEAD,
    TOOL_CONTENT_TAIL,
    HTML_COMMENT,
};

typedef struct {
    char tool_id[256];
    bool in_tool_content;
    bool expecting_tail; // true when we've scanned head and now need tail
} Scanner;

static inline void advance(TSLexer *lexer) { lexer->advance(lexer, false); }

static inline void skip(TSLexer *lexer) { lexer->advance(lexer, true); }

unsigned tree_sitter_greger_external_scanner_serialize(void *payload, char *buffer) {
    Scanner *scanner = (Scanner *)payload;

    size_t tool_id_len = strlen(scanner->tool_id);
    if (tool_id_len >= 255) tool_id_len = 255;

    buffer[0] = scanner->in_tool_content ? 1 : 0;
    buffer[1] = scanner->expecting_tail ? 1 : 0;
    buffer[2] = tool_id_len;
    memcpy(buffer + 3, scanner->tool_id, tool_id_len);

    return 3 + tool_id_len;
}

void tree_sitter_greger_external_scanner_deserialize(void *payload, const char *buffer, unsigned length) {
    Scanner *scanner = (Scanner *)payload;

    if (length == 0) {
        scanner->in_tool_content = false;
        scanner->expecting_tail = false;
        scanner->tool_id[0] = '\0';
        return;
    }

    scanner->in_tool_content = buffer[0] == 1;
    if (length > 1) {
        scanner->expecting_tail = buffer[1] == 1;
        if (length > 2) {
            unsigned tool_id_len = buffer[2];
            if (tool_id_len >= 255) tool_id_len = 255;
            if (length >= 3 + tool_id_len) {
                memcpy(scanner->tool_id, buffer + 3, tool_id_len);
                scanner->tool_id[tool_id_len] = '\0';
            } else {
                scanner->tool_id[0] = '\0';
            }
        } else {
            scanner->tool_id[0] = '\0';
        }
    } else {
        scanner->expecting_tail = false;
        scanner->tool_id[0] = '\0';
    }
}

void *tree_sitter_greger_external_scanner_create(void) {
    Scanner *scanner = malloc(sizeof(Scanner));
    scanner->in_tool_content = false;
    scanner->expecting_tail = false;
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
    unsigned dashes = 0;
    while (lexer->lookahead) {
        switch (lexer->lookahead) {
            case '-':
                ++dashes;
                break;
            case '>':
                if (dashes >= 2) {
                    lexer->result_symbol = HTML_COMMENT;
                    advance(lexer);
                    return true;
                }
                // fallthrough
            default:
                dashes = 0;
        }
        advance(lexer);
    }

    return false;
}

static bool scan_tool_start_tag(Scanner *scanner, TSLexer *lexer) {
    if (lexer->lookahead != '<') return false;
    advance(lexer);

    // Check for "tool."
    if (lexer->lookahead != 't') return false;
    advance(lexer);
    if (lexer->lookahead != 'o') return false;
    advance(lexer);
    if (lexer->lookahead != 'o') return false;
    advance(lexer);
    if (lexer->lookahead != 'l') return false;
    advance(lexer);
    if (lexer->lookahead != '.') return false;
    advance(lexer);

    // Get the tool ID and store it
    int id_len = 0;
    while (lexer->lookahead != '>' && lexer->lookahead != 0 && id_len < 255) {
        scanner->tool_id[id_len++] = lexer->lookahead;
        advance(lexer);
    }
    scanner->tool_id[id_len] = '\0';

    if (lexer->lookahead != '>') return false;
    advance(lexer);

    scanner->in_tool_content = true;
    scanner->expecting_tail = false;
    lexer->result_symbol = TOOL_START_TAG;
    return true;
}

static bool scan_tool_end_tag(Scanner *scanner, TSLexer *lexer) {
    if (lexer->lookahead != '<') return false;
    advance(lexer);

    if (lexer->lookahead != '/') return false;
    advance(lexer);

    // Check for "tool."
    if (lexer->lookahead != 't') return false;
    advance(lexer);
    if (lexer->lookahead != 'o') return false;
    advance(lexer);
    if (lexer->lookahead != 'o') return false;
    advance(lexer);
    if (lexer->lookahead != 'l') return false;
    advance(lexer);
    if (lexer->lookahead != '.') return false;
    advance(lexer);

    // Scan any tool ID until >
    while (lexer->lookahead != '>' && lexer->lookahead != 0) {
        advance(lexer);
    }

    if (lexer->lookahead != '>') return false;
    advance(lexer);

    scanner->in_tool_content = false;
    scanner->expecting_tail = false;
    scanner->tool_id[0] = '\0';
    lexer->result_symbol = TOOL_END_TAG;
    return true;
}

static bool scan_tool_content_head(Scanner *scanner, TSLexer *lexer) {
    if (!scanner->in_tool_content || scanner->expecting_tail) return false;

    lexer->mark_end(lexer);

    // Build the expected closing tag
    char expected_closing[512];
    snprintf(expected_closing, sizeof(expected_closing), "</tool.%s>", scanner->tool_id);
    int expected_len = strlen(expected_closing);

    int match_index = 0;
    bool has_content = false;
    int line_count = 0;

    // Scan first 4 lines or until we find the closing tag
    while (lexer->lookahead != 0 && line_count < 4) {
        if (lexer->lookahead == expected_closing[match_index]) {
            match_index++;
            if (match_index == expected_len) {
                // Found complete closing tag, stop here (don't consume it)
                if (has_content) {
                    lexer->result_symbol = TOOL_CONTENT_HEAD;
                    return true;
                } else {
                    // No content, let the grammar handle the end tag
                    return false;
                }
            }
            advance(lexer);
        } else {
            // Reset match and continue as content
            if (match_index > 0) {
                // We were partially matching, reset but don't advance yet
                match_index = 0;
                // Don't advance here, reprocess this character
            } else {
                if (lexer->lookahead == '\n') {
                    line_count++;
                }
                advance(lexer);
                has_content = true;
                lexer->mark_end(lexer);
            }
        }
    }

    // If we reached 4 lines and have content, return head and expect tail next
    if (has_content) {
        if (line_count >= 4) {
            scanner->expecting_tail = true;
        }
        lexer->result_symbol = TOOL_CONTENT_HEAD;
        return true;
    }

    return false;
}

static bool scan_tool_content_tail(Scanner *scanner, TSLexer *lexer) {
    if (!scanner->in_tool_content || !scanner->expecting_tail) return false;

    lexer->mark_end(lexer);

    // Build the expected closing tag
    char expected_closing[512];
    snprintf(expected_closing, sizeof(expected_closing), "</tool.%s>", scanner->tool_id);
    int expected_len = strlen(expected_closing);

    int match_index = 0;
    bool has_content = false;

    // Scan remaining content until we find the closing tag
    while (lexer->lookahead != 0) {
        if (lexer->lookahead == expected_closing[match_index]) {
            match_index++;
            if (match_index == expected_len) {
                // Found complete closing tag, stop here (don't consume it)
                scanner->expecting_tail = false;
                if (has_content) {
                    lexer->result_symbol = TOOL_CONTENT_TAIL;
                    return true;
                } else {
                    // No tail content, let the grammar handle the end tag
                    return false;
                }
            }
            advance(lexer);
        } else {
            // Reset match and continue as content
            if (match_index > 0) {
                // We were partially matching, reset but don't advance yet
                match_index = 0;
                // Don't advance here, reprocess this character
            } else {
                advance(lexer);
                has_content = true;
                lexer->mark_end(lexer);
            }
        }
    }

    // Reached end without finding closing tag
    scanner->expecting_tail = false;
    if (has_content) {
        lexer->result_symbol = TOOL_CONTENT_TAIL;
        return true;
    }

    return false;
}

bool tree_sitter_greger_external_scanner_scan(void *payload, TSLexer *lexer, const bool *valid_symbols) {
    Scanner *scanner = (Scanner *)payload;

    // Handle tool content (raw text) when in tool content state
    if (valid_symbols[TOOL_CONTENT] && scanner->in_tool_content) {
        return scan_tool_content(scanner, lexer);
    }

    // Skip whitespace but preserve newlines for other tokens
    while (iswspace(lexer->lookahead) && lexer->lookahead != '\n') {
        skip(lexer);
    }

    if (lexer->lookahead == '<') {
        // Handle HTML comments first
        if (valid_symbols[HTML_COMMENT]) {
            return scan_html_comment(lexer);
        }

        // Handle tool start tag
        if (valid_symbols[TOOL_START_TAG]) {
            return scan_tool_start_tag(scanner, lexer);
        }

        // Handle tool end tag
        if (valid_symbols[TOOL_END_TAG]) {
            return scan_tool_end_tag(scanner, lexer);
        }
    }

    return false;
}

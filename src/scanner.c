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
    EVAL_CONTENT,
    ERROR_SENTINEL,
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
    strncpy(expected_closing, "</tool.", sizeof(expected_closing) - 1);
    expected_closing[sizeof(expected_closing) - 1] = '\0';
    strncat(expected_closing, scanner->tool_id, sizeof(expected_closing) - strlen(expected_closing) - 1);
    strncat(expected_closing, ">", sizeof(expected_closing) - strlen(expected_closing) - 1);
    int expected_len = strlen(expected_closing);

    int match_index = 0;
    bool has_content = false;
    int line_count = 0;
    bool current_line_has_content = false;

    // Scan first 4 lines or until we find the closing tag
    while (lexer->lookahead != 0) {
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
            current_line_has_content = true;
        } else {
            // Reset match and continue as content
            if (match_index > 0) {
                // We were partially matching but failed, reset and process current char as content
                match_index = 0;
            }
            
            // Process current character as content
            if (lexer->lookahead == '\n') {
                if (current_line_has_content) {
                    line_count++;
                    current_line_has_content = false;
                }
                advance(lexer);
                has_content = true;
                lexer->mark_end(lexer);
                
                // If we've completed 4 lines, break and let natural flow determine if tail exists
                if (line_count >= 4) {
                    break;
                }
            } else {
                current_line_has_content = true;
                advance(lexer);
                has_content = true;
                lexer->mark_end(lexer);
            }
        }
    }

    // Return head if we have content
    if (has_content) {
        // If we broke out because we reached 4 lines, check if there's more content
        if (line_count >= 4) {
            // Check if current position is at the closing tag
            int temp_match = 0;
            while (lexer->lookahead == expected_closing[temp_match] && temp_match < expected_len) {
                temp_match++;
                if (temp_match == expected_len) {
                    // We're at the closing tag, no tail needed
                    break;
                }
                lexer->advance(lexer, false);
            }
            
            if (temp_match != expected_len) {
                // We're not at the closing tag, so there must be more content
                scanner->expecting_tail = true;
            }
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
    strncpy(expected_closing, "</tool.", sizeof(expected_closing) - 1);
    expected_closing[sizeof(expected_closing) - 1] = '\0';
    strncat(expected_closing, scanner->tool_id, sizeof(expected_closing) - strlen(expected_closing) - 1);
    strncat(expected_closing, ">", sizeof(expected_closing) - strlen(expected_closing) - 1);
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



static bool scan_eval_content(TSLexer *lexer) {
    // Only scan for content that is actually between eval tags
    lexer->mark_end(lexer);
    
    while (lexer->lookahead) {
        if (lexer->lookahead == '<') {
            // Check if this is the closing tag
            TSLexer saved_lexer = *lexer;
            advance(lexer);
            if (lexer->lookahead == '/') {
                advance(lexer);
                if (lexer->lookahead == 'e') {
                    advance(lexer);
                    if (lexer->lookahead == 'v') {
                        advance(lexer);
                        if (lexer->lookahead == 'a') {
                            advance(lexer);
                            if (lexer->lookahead == 'l') {
                                // Found closing tag, stop here and restore lexer
                                *lexer = saved_lexer;
                                lexer->result_symbol = EVAL_CONTENT;
                                return true;
                            }
                        }
                    }
                }
            }
            // Not closing tag, restore and continue
            *lexer = saved_lexer;
        }
        
        advance(lexer);
        lexer->mark_end(lexer);
    }
    
    // If we reached EOF without finding closing tag, return content
    lexer->result_symbol = EVAL_CONTENT;
    return true;
}

bool tree_sitter_greger_external_scanner_scan(void *payload, TSLexer *lexer, const bool *valid_symbols) {
    Scanner *scanner = (Scanner *)payload;

    // Check for error recovery mode
    if (valid_symbols[ERROR_SENTINEL]) {
        return false;
    }

    // Handle tool content (raw text) when in tool content state
    if (scanner->in_tool_content) {
        if (valid_symbols[TOOL_CONTENT_HEAD] && !scanner->expecting_tail) {
            return scan_tool_content_head(scanner, lexer);
        }
        if (valid_symbols[TOOL_CONTENT_TAIL] && scanner->expecting_tail) {
            return scan_tool_content_tail(scanner, lexer);
        }
    }

    // Skip whitespace but preserve newlines for other tokens
    while (iswspace(lexer->lookahead) && lexer->lookahead != '\n') {
        skip(lexer);
    }

    if (lexer->lookahead == '<') {
        // Handle HTML comments first - they should have priority
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

    // Handle eval content
    if (valid_symbols[EVAL_CONTENT]) {
        return scan_eval_content(lexer);
    }


    
    return false;
}



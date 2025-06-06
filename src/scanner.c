#include <tree_sitter/parser.h>
#include <string.h>
#include <wctype.h>

enum TokenType {
  TOOL_BLOCK_START,
  TOOL_BLOCK_END,
  TOOL_BLOCK_CONTENT,
};

typedef struct {
  char tool_id[64];
  bool in_tool_block;
} Scanner;

static void advance(TSLexer *lexer) {
  lexer->advance(lexer, false);
}

static void skip(TSLexer *lexer) {
  lexer->advance(lexer, true);
}

void *tree_sitter_greger_external_scanner_create() {
  Scanner *scanner = calloc(1, sizeof(Scanner));
  return scanner;
}

void tree_sitter_greger_external_scanner_destroy(void *payload) {
  Scanner *scanner = (Scanner *)payload;
  free(scanner);
}

unsigned tree_sitter_greger_external_scanner_serialize(void *payload, char *buffer) {
  Scanner *scanner = (Scanner *)payload;
  if (scanner->in_tool_block) {
    size_t len = strlen(scanner->tool_id);
    buffer[0] = 1; // in_tool_block = true
    memcpy(buffer + 1, scanner->tool_id, len);
    return len + 1;
  } else {
    buffer[0] = 0; // in_tool_block = false
    return 1;
  }
}

void tree_sitter_greger_external_scanner_deserialize(void *payload, const char *buffer, unsigned length) {
  Scanner *scanner = (Scanner *)payload;
  if (length > 0 && buffer[0] == 1) {
    scanner->in_tool_block = true;
    memcpy(scanner->tool_id, buffer + 1, length - 1);
    scanner->tool_id[length - 1] = '\0';
  } else {
    scanner->in_tool_block = false;
    scanner->tool_id[0] = '\0';
  }
}

static bool scan_tool_start(TSLexer *lexer, Scanner *scanner) {
  // Expect "<tool."
  if (lexer->lookahead != '<') return false;
  advance(lexer);

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

  // Read the tool ID
  int id_len = 0;
  while (lexer->lookahead &&
         (isalnum(lexer->lookahead) || lexer->lookahead == '_' ||
          lexer->lookahead == '-' || lexer->lookahead == '.') &&
         id_len < 63) {
    scanner->tool_id[id_len++] = lexer->lookahead;
    advance(lexer);
  }

  if (lexer->lookahead != '>') return false;
  advance(lexer);

  scanner->tool_id[id_len] = '\0';
  scanner->in_tool_block = true;

  lexer->result_symbol = TOOL_BLOCK_START;
  return true;
}

static bool scan_tool_end(TSLexer *lexer, Scanner *scanner) {
  // Expect "</tool."
  if (lexer->lookahead != '<') return false;
  advance(lexer);

  if (lexer->lookahead != '/') return false;
  advance(lexer);

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

  // Read the tool ID and check if it matches
  char end_id[64];
  int id_len = 0;
  while (lexer->lookahead &&
         (isalnum(lexer->lookahead) || lexer->lookahead == '_' ||
          lexer->lookahead == '-' || lexer->lookahead == '.') &&
         id_len < 63) {
    end_id[id_len++] = lexer->lookahead;
    advance(lexer);
  }

  if (lexer->lookahead != '>') return false;
  advance(lexer);

  end_id[id_len] = '\0';

  // Only match if the IDs are the same
  if (strcmp(scanner->tool_id, end_id) == 0) {
    scanner->in_tool_block = false;
    scanner->tool_id[0] = '\0';
    lexer->result_symbol = TOOL_BLOCK_END;
    return true;
  }

  return false;
}

static bool scan_tool_content(TSLexer *lexer, Scanner *scanner) {
  if (!scanner->in_tool_block) return false;

  // Skip any leading whitespace on the same line
  while (lexer->lookahead == ' ' || lexer->lookahead == '\t') {
    skip(lexer);
  }

  // If we hit a newline immediately, don't consume it as content
  if (lexer->lookahead == '\n') {
    return false;
  }

  // Scan until we hit a potential end tag or newline
  bool has_content = false;
  while (lexer->lookahead && lexer->lookahead != '\n') {
    // Check if we're at the start of a potential closing tag
    if (lexer->lookahead == '<') {
      // Look ahead to see if this is our closing tag
      TSLexer saved_lexer = *lexer;
      Scanner saved_scanner = *scanner;

      if (scan_tool_end(lexer, scanner)) {
        // This is our closing tag, don't consume it
        *lexer = saved_lexer;
        *scanner = saved_scanner;
        break;
      } else {
        // Not our closing tag, restore and continue
        *lexer = saved_lexer;
        *scanner = saved_scanner;
        advance(lexer);
        has_content = true;
      }
    } else {
      advance(lexer);
      has_content = true;
    }
  }

  if (has_content) {
    lexer->result_symbol = TOOL_BLOCK_CONTENT;
    return true;
  }

  return false;
}

bool tree_sitter_greger_external_scanner_scan(void *payload, TSLexer *lexer, const bool *valid_symbols) {
  Scanner *scanner = (Scanner *)payload;

  // Skip whitespace
  while (iswspace(lexer->lookahead)) {
    skip(lexer);
  }

  if (valid_symbols[TOOL_BLOCK_START] && !scanner->in_tool_block) {
    return scan_tool_start(lexer, scanner);
  }

  if (valid_symbols[TOOL_BLOCK_END] && scanner->in_tool_block) {
    return scan_tool_end(lexer, scanner);
  }

  if (valid_symbols[TOOL_BLOCK_CONTENT] && scanner->in_tool_block) {
    return scan_tool_content(lexer, scanner);
  }

  return false;
}

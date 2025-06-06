#include <tree_sitter/parser.h>
#include <tree_sitter/alloc.h>
#include <string.h>
#include <wctype.h>

enum TokenType {
  TOOL_BLOCK_START,
  TOOL_BLOCK_END,
  TOOL_BLOCK_CONTENT,
};

typedef struct {
  char current_tool_id[64];
  bool in_tool_block;
} Scanner;

static void advance(TSLexer *lexer) {
  lexer->advance(lexer, false);
}

static void skip(TSLexer *lexer) {
  lexer->advance(lexer, true);
}

void *tree_sitter_greger_external_scanner_create() {
  Scanner *scanner = ts_malloc(sizeof(Scanner));
  scanner->current_tool_id[0] = '\0';
  scanner->in_tool_block = false;
  return scanner;
}

void tree_sitter_greger_external_scanner_destroy(void *payload) {
  Scanner *scanner = (Scanner *)payload;
  ts_free(scanner);
}

unsigned tree_sitter_greger_external_scanner_serialize(void *payload, char *buffer) {
  Scanner *scanner = (Scanner *)payload;
  size_t id_len = strlen(scanner->current_tool_id);

  buffer[0] = scanner->in_tool_block ? 1 : 0;
  buffer[1] = (char)id_len;
  memcpy(buffer + 2, scanner->current_tool_id, id_len);

  return 2 + id_len;
}

void tree_sitter_greger_external_scanner_deserialize(void *payload, const char *buffer, unsigned length) {
  Scanner *scanner = (Scanner *)payload;

  if (length < 2) {
    scanner->in_tool_block = false;
    scanner->current_tool_id[0] = '\0';
    return;
  }

  scanner->in_tool_block = buffer[0] == 1;
  size_t id_len = (size_t)buffer[1];

  if (length >= 2 + id_len && id_len < 63) {
    memcpy(scanner->current_tool_id, buffer + 2, id_len);
    scanner->current_tool_id[id_len] = '\0';
  } else {
    scanner->current_tool_id[0] = '\0';
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
    scanner->current_tool_id[id_len++] = lexer->lookahead;
    advance(lexer);
  }

  if (lexer->lookahead != '>') return false;
  advance(lexer);

  scanner->current_tool_id[id_len] = '\0';
  scanner->in_tool_block = true;

  lexer->result_symbol = TOOL_BLOCK_START;
  return true;
}

static bool scan_tool_end(TSLexer *lexer, Scanner *scanner) {
  if (!scanner->in_tool_block) return false;

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

  // Read the tool ID and check if it matches our current one
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

  // Only match if the IDs are the same as our current tool ID
  if (strcmp(scanner->current_tool_id, end_id) == 0) {
    scanner->in_tool_block = false;
    scanner->current_tool_id[0] = '\0';
    lexer->result_symbol = TOOL_BLOCK_END;
    return true;
  }

  return false;
}

static bool scan_tool_content(TSLexer *lexer, Scanner *scanner) {
  if (!scanner->in_tool_block) return false;

  bool has_content = false;

  // Simple approach: consume until we see '<'
  while (lexer->lookahead && lexer->lookahead != '<') {
    advance(lexer);
    has_content = true;
  }

  if (has_content) {
    lexer->result_symbol = TOOL_BLOCK_CONTENT;
    return true;
  }

  return false;
}

bool tree_sitter_greger_external_scanner_scan(void *payload, TSLexer *lexer, const bool *valid_symbols) {
  Scanner *scanner = (Scanner *)payload;

  // Handle tool block content first (don't skip whitespace for content)
  if (valid_symbols[TOOL_BLOCK_CONTENT] && scanner->in_tool_block) {
    return scan_tool_content(lexer, scanner);
  }

  // Skip whitespace for start/end tokens
  while (iswspace(lexer->lookahead)) {
    skip(lexer);
  }

  if (valid_symbols[TOOL_BLOCK_START] && !scanner->in_tool_block) {
    return scan_tool_start(lexer, scanner);
  }

  if (valid_symbols[TOOL_BLOCK_END] && scanner->in_tool_block) {
    return scan_tool_end(lexer, scanner);
  }

  return false;
}

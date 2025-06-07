/**
 * @file greger.el parser - Tree-sitter grammar for greger conversation format
 * @author Andreas Jansson <andreas.s.t.jansson@gmail.com>
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: "greger",

  extras: $ => [
    /[ \t]/,
  ],

  externals: $ => [
    $.tool_block_start,
    $.tool_block_end,
    $.tool_block_content,
    $._text,
  ],

  conflicts: $ => [
  ],

  rules: {
    source_file: $ => repeat($.section),

    section: $ => choice(
      $.user_section,
      $.system_section,
      $.assistant_section,
      $.thinking_section,
      $.tool_use_section,
      $.tool_result_section,
      $.server_tool_use_section,
      $.server_tool_result_section,
      $.citations_section,
    ),

    user_section: $ => seq(
      $.user_header,
      optional(alias($.content, $.section_content))
    ),

    system_section: $ => seq(
      $.system_header,
      optional(alias($.system_content, $.section_content))
    ),

    assistant_section: $ => seq(
      $.assistant_header,
      optional(alias($.content, $.section_content))
    ),

    thinking_section: $ => seq(
      $.thinking_header,
      optional(alias($.content, $.section_content))
    ),

    tool_use_section: $ => seq(
      $.tool_use_header,
      optional($.tool_use_content)
    ),

    tool_result_section: $ => seq(
      $.tool_result_header,
      optional($.tool_result_content)
    ),

    server_tool_use_section: $ => seq(
      $.server_tool_use_header,
      optional($.tool_use_content)
    ),

    server_tool_result_section: $ => seq(
      $.server_tool_result_header,
      optional($.tool_result_content)
    ),

    // Citations without text - standalone citations section
    citations_section: $ => alias($.citations_without_text, 'citations_without_text'),

    citations_without_text: $ => seq(
      $.citations_header,
      repeat1(choice(
        field("entry", $.citation_entry),
        $.newline
      ))
    ),

    // Headers - simple tokens
    user_header: $ => /##[ \t]*USER:[ \t]*\n/,
    system_header: $ => /##[ \t]*SYSTEM:[ \t]*\n/,
    assistant_header: $ => /##[ \t]*ASSISTANT:[ \t]*\n/,
    thinking_header: $ => /##[ \t]*THINKING:[ \t]*\n/,
    tool_use_header: $ => /##[ \t]*TOOL USE:[ \t]*\n/,
    tool_result_header: $ => /##[ \t]*TOOL RESULT:[ \t]*\n/,
    server_tool_use_header: $ => /##[ \t]*SERVER TOOL USE:[ \t]*\n/,
    server_tool_result_header: $ => /##[ \t]*SERVER TOOL RESULT:[ \t]*\n/,
    citations_header: $ => /##[ \t]*CITATIONS:[ \t]*\n/,

    // Content types - simple content lines
    content: $ => repeat1(choice(
      $.content_line,
      $.newline
    )),

    content_line: $ => seq(
      repeat1(choice(
        alias($._text, 'text'),
        $.cite_tag_inline
      )),
      "\n"
    ),

    // Inline cite tag (doesn't trigger citations_with_text by itself)
    cite_tag_inline: $ => seq(
      "<cite>",
      repeat1(/[^<\n]+/),
      "</cite>"
    ),

    // Citations with text - complete section spanning from content to citations
    citations_with_text: $ => prec.right(seq(
      // Content that includes cite tags
      repeat1(choice(
        alias(/[^<#\n]+/, 'text'),
        seq(
          "<cite>",
          field("text", repeat1(/[^<\n]+/)),
          "</cite>"
        ),
        /[ \t]*\n/
      )),
      // Citations section
      "##",
      /[ \t]*/,
      "CITATIONS:",
      /[ \t]*/,
      "\n",
      repeat1(choice(
        field("entry", $.citation_entry),
        $.newline
      ))
    )),

    system_content: $ => repeat1(choice(
      $.content_line,
      $.newline
    )),

    tool_use_content: $ => repeat1(choice(
      $.tool_name_line,
      $.tool_id_line,
      $.tool_parameter,
      $.newline
    )),

    tool_result_content: $ => repeat1(choice(
      $.tool_result_id_line,
      $.tool_result_block,
      $.newline
    )),

    citations_content: $ => repeat1(choice(
      $.citation_entry,
      $.newline
    )),







    // Tool-specific patterns
    tool_name_line: $ => seq(
      "Name:",
      /[ \t]*/,
      field("name", $.identifier),
      "\n"
    ),

    tool_id_line: $ => seq(
      "ID:",
      /[ \t]*/,
      field("id", $.identifier),
      "\n"
    ),

    tool_parameter: $ => seq(
      "###",
      /[ \t]*/,
      field("param_name", $.identifier),
      "\n",
      "\n",
      field("param_value", $.tool_param_block)
    ),

    tool_param_block: $ => seq(
      $.tool_block_start,
      field("content", repeat($.tool_block_content)),
      $.tool_block_end
    ),

    tool_result_id_line: $ => seq(
      "ID:",
      /[ \t]*/,
      field("id", $.identifier),
      "\n"
    ),

    tool_result_block: $ => seq(
      $.tool_block_start,
      field("content", repeat($.tool_block_content)),
      $.tool_block_end
    ),

    // Citation patterns
    citation_entry: $ => prec.left(seq(
      "###",
      /[ \t]*/,
      field("url", $.url),
      "\n",
      "\n",
      repeat(choice(
        $.citation_title,
        $.citation_text,
        $.citation_index,
        $.newline
      ))
    )),

    citation_title: $ => seq(
      "Title:",
      /[ \t]*/,
      field("title", /[^\n]+/),
      "\n"
    ),

    citation_text: $ => seq(
      "Cited text:",
      /[ \t]*/,
      field("text", /[^\n]+/),
      "\n"
    ),

    citation_index: $ => seq(
      "Encrypted index:",
      /[ \t]*/,
      field("index", /[^\n]+/),
      "\n"
    ),





    // Tool content line - anything except closing tool tag
    tool_content_line: $ => prec(-1, /[^\n]+/),

    newline: $ => "\n",

    // Basic tokens
    identifier: $ => /[a-zA-Z_][a-zA-Z0-9_.-]*/,
    url: $ => /https?:\/\/[^\s\n]+/,
  }
});

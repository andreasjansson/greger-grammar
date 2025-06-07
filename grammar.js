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
  ],

  rules: {
    document: $ => repeat($._item),

    _item: $ => choice(
      $.section,
      $.content_line,
    ),

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

    user_section: $ => /##[ \t]*USER:[ \t]*\n/,
    system_section: $ => /##[ \t]*SYSTEM:[ \t]*\n/,
    assistant_section: $ => /##[ \t]*ASSISTANT:[ \t]*\n/,
    thinking_section: $ => /##[ \t]*THINKING:[ \t]*\n/,
    tool_use_section: $ => /##[ \t]*TOOL USE:[ \t]*\n/,
    tool_result_section: $ => /##[ \t]*TOOL RESULT:[ \t]*\n/,
    server_tool_use_section: $ => /##[ \t]*SERVER TOOL USE:[ \t]*\n/,
    server_tool_result_section: $ => /##[ \t]*SERVER TOOL RESULT:[ \t]*\n/,
    citations_section: $ => /##[ \t]*CITATIONS:[ \t]*\n/,

    content_line: $ => choice(
      $.text_line,
      $.cite_line,
      $.code_line,
      $.tool_name_line,
      $.tool_id_line,
      $.tool_parameter,
      $.tool_result_block,
      $.citation_entry,
      $.citation_field,
      $.empty_line,
    ),

    text_line: $ => seq(
      /[^\n#][^\n]*/,
      "\n"
    ),

    cite_line: $ => seq(
      optional(/[^\n#<][^\n<]*/),
      $.cite_tag,
      optional(/[^\n<]*/),
      "\n"
    ),

    cite_tag: $ => seq(
      "<cite>",
      field("text", /[^<]+/),
      "</cite>"
    ),

    code_line: $ => seq(
      "```",
      optional(/[^\n]*/),
      "\n"
    ),

    empty_line: $ => "\n",

    // Tool-specific content
    tool_name_line: $ => seq(
      "Name:",
      /[ \t]*/,
      field("name", /[^\n]+/),
      "\n"
    ),

    tool_id_line: $ => seq(
      "ID:",
      /[ \t]*/,
      field("id", /[^\n]+/),
      "\n"
    ),

    tool_parameter: $ => seq(
      "###",
      /[ \t]*/,
      field("name", /[^\n]+/),
      "\n",
      "\n",
      $.tool_parameter_block
    ),

    tool_parameter_block: $ => seq(
      $.tool_block_start,
      repeat($.tool_block_content),
      optional($.tool_block_end)
    ),

    tool_result_block: $ => seq(
      $.tool_block_start,
      repeat($.tool_block_content),
      optional($.tool_block_end)
    ),

    // Citation content
    citation_entry: $ => seq(
      "###",
      /[ \t]*/,
      field("url", /[^\n]+/),
      "\n"
    ),

    citation_field: $ => choice(
      $.citation_title,
      $.citation_text,
      $.citation_index,
    ),

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
  }
});

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
    document: $ => repeat($.section),

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
      $.untagged_content,
    ),

    user_section: $ => prec.left(seq(
      /##[ \t]*USER:[ \t]*\n/,
      repeat($.content_line)
    )),

    system_section: $ => prec.left(seq(
      /##[ \t]*SYSTEM:[ \t]*\n/,
      repeat($.content_line)
    )),

    assistant_section: $ => prec.left(seq(
      /##[ \t]*ASSISTANT:[ \t]*\n/,
      repeat($.content_line)
    )),

    thinking_section: $ => prec.left(seq(
      /##[ \t]*THINKING:[ \t]*\n/,
      repeat($.content_line)
    )),

    tool_use_section: $ => prec.left(seq(
      /##[ \t]*TOOL USE:[ \t]*\n/,
      repeat(choice(
        $.tool_name_line,
        $.tool_id_line,
        $.tool_parameter,
        $.content_line,
      ))
    )),

    tool_result_section: $ => prec.left(seq(
      /##[ \t]*TOOL RESULT:[ \t]*\n/,
      repeat(choice(
        $.tool_result_id_line,
        $.tool_result_block,
        $.content_line,
      ))
    )),

    server_tool_use_section: $ => prec.left(seq(
      /##[ \t]*SERVER TOOL USE:[ \t]*\n/,
      repeat(choice(
        $.tool_name_line,
        $.tool_id_line,
        $.tool_parameter,
        $.content_line,
      ))
    )),

    server_tool_result_section: $ => prec.left(seq(
      /##[ \t]*SERVER TOOL RESULT:[ \t]*\n/,
      repeat(choice(
        $.tool_result_id_line,
        $.tool_result_block,
        $.content_line,
      ))
    )),

    citations_section: $ => prec.left(seq(
      /##[ \t]*CITATIONS:[ \t]*\n/,
      repeat(choice(
        $.citation_entry,
        $.content_line,
      ))
    )),

    // Content line types
    content_line: $ => choice(
      $.text_line,
      $.cite_line,
      $.code_block_line,
      $.empty_line,
    ),

    text_line: $ => seq(
      /[^#\n<][^\n]*/,
      "\n"
    ),

    cite_line: $ => seq(
      optional(/[^#\n<][^\n<]*/),
      $.cite_tag,
      optional(/[^\n<]*/),
      "\n"
    ),

    cite_tag: $ => seq(
      "<cite>",
      field("text", /[^<]+/),
      "</cite>"
    ),

    code_block_line: $ => seq(
      "```",
      optional(/[^\n]*/),
      "\n"
    ),

    empty_line: $ => "\n",

    // Tool-specific elements
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

    tool_result_id_line: $ => seq(
      "ID:",
      /[ \t]*/,
      field("id", /[^\n]+/),
      "\n"
    ),

    tool_result_block: $ => seq(
      $.tool_block_start,
      repeat($.tool_block_content),
      optional($.tool_block_end)
    ),

    // Citations content
    citation_entry: $ => seq(
      "###",
      /[ \t]*/,
      field("url", /[^\n]+/),
      "\n",
      "\n",
      repeat(choice(
        $.citation_title,
        $.citation_text,
        $.citation_index,
        $.content_line,
      ))
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

    // Untagged content at the beginning
    untagged_content: $ => prec.left(seq(
      $.text_line,
      repeat($.content_line)
    )),
  }
});

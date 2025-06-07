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
      $.line,
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

    user_section: $ => prec.left(seq(
      /##[ \t]*USER:[ \t]*\n/,
      repeat($.line)
    )),

    system_section: $ => prec.left(seq(
      /##[ \t]*SYSTEM:[ \t]*\n/,
      repeat($.line)
    )),

    assistant_section: $ => prec.left(seq(
      /##[ \t]*ASSISTANT:[ \t]*\n/,
      repeat($.line)
    )),

    thinking_section: $ => prec.left(seq(
      /##[ \t]*THINKING:[ \t]*\n/,
      repeat($.line)
    )),

    tool_use_section: $ => prec.left(seq(
      /##[ \t]*TOOL USE:[ \t]*\n/,
      repeat(choice(
        $.tool_line,
        $.line,
      ))
    )),

    tool_result_section: $ => prec.left(seq(
      /##[ \t]*TOOL RESULT:[ \t]*\n/,
      repeat(choice(
        $.tool_line,
        $.line,
      ))
    )),

    server_tool_use_section: $ => prec.left(seq(
      /##[ \t]*SERVER TOOL USE:[ \t]*\n/,
      repeat(choice(
        $.tool_line,
        $.line,
      ))
    )),

    server_tool_result_section: $ => prec.left(seq(
      /##[ \t]*SERVER TOOL RESULT:[ \t]*\n/,
      repeat(choice(
        $.tool_line,
        $.line,
      ))
    )),

    citations_section: $ => prec.left(seq(
      /##[ \t]*CITATIONS:[ \t]*\n/,
      repeat(choice(
        $.citation_line,
        $.line,
      ))
    )),

    // Basic line types
    line: $ => choice(
      $.text_line,
      $.cite_line,
      $.code_line,
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

    // Tool-specific lines
    tool_line: $ => choice(
      $.tool_name_line,
      $.tool_id_line,
      $.tool_parameter,
      $.tool_result_block,
    ),

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

    // Citation-specific lines
    citation_line: $ => choice(
      $.citation_entry_line,
      $.citation_title_line,
      $.citation_text_line,
      $.citation_index_line,
    ),

    citation_entry_line: $ => seq(
      "###",
      /[ \t]*/,
      field("url", /[^\n]+/),
      "\n"
    ),

    citation_title_line: $ => seq(
      "Title:",
      /[ \t]*/,
      field("title", /[^\n]+/),
      "\n"
    ),

    citation_text_line: $ => seq(
      "Cited text:",
      /[ \t]*/,
      field("text", /[^\n]+/),
      "\n"
    ),

    citation_index_line: $ => seq(
      "Encrypted index:",
      /[ \t]*/,
      field("index", /[^\n]+/),
      "\n"
    ),
  }
});

/**
 * @file Greger grammar for tree-sitter
 * @author Andreas Jansson
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: "greger",

  extras: $ => [
    /[ \t\n]/,
  ],

  externals: $ => [
    $.tool_start_tag,
    $.tool_end_tag,
    $.tool_content,
    $.html_comment,
  ],

  inline: $ => [
    $.content_blocks,
    $.assistant_content_blocks,
    $.system_content_blocks,
  ],



  conflicts: $ => [
    [$.user, $.text],
    [$.assistant, $.text],
    [$.system, $.text],
    [$.thinking, $.text],
  ],

  rules: {

    source_file: $ => seq(
      optional($.untagged_text),
      optional($.system),
      repeat($._block),
    ),

    untagged_text: $ => prec(-1,
      repeat1(seq(
        $._untagged_text_content,
        "\n"
      ))
    ),

    _block: $ => choice(
      $.user,
      $.assistant,
      $.thinking,
      $.tool_use,
      $.tool_result,
      $.server_tool_use,
      $.web_search_tool_result,
      $.citations,
    ),

    user: $ => seq(
      $.user_header,
      optional(/\n/),
      $.content_blocks,
    ),

    assistant: $ => seq(
      $.assistant_header,
      optional(/\n/),
      $.assistant_content_blocks,
    ),

    system: $ => seq(
      $.system_header,
      optional(/\n/),
      $.system_content_blocks,
    ),

    thinking: $ => seq(
      $.thinking_header,
      optional(/\n/),
      $.content_blocks,
    ),

    tool_use: $ => seq(
      $.tool_use_header,
      optional(/\n/),
      repeat(choice(
        $.name,
        $.id,
        $.tool_param,
      )),
    ),

    tool_result: $ => seq(
      $.tool_result_header,
      optional(/\n/),
      $.id,
      optional(/\n/),
      $.content,
    ),

    server_tool_use: $ => seq(
      $.server_tool_use_header,
      optional(/\n/),
      repeat(choice(
        $.name,
        $.id,
        $.tool_param,
      )),
    ),

    web_search_tool_result: $ => seq(
      $.web_search_tool_result_header,
      optional(/\n/),
      $.id,
      optional(/\n/),
      $.content,
    ),

    citations: $ => seq(
      $.citations_header,
      repeat($.citation_entry),
    ),

    user_header: $ => token(seq('##', /[ \t]*/, 'USER:\n')),

    assistant_header: $ => token(seq('##', /[ \t]*/, 'ASSISTANT:\n')),

    system_header: $ => token(seq('##', /[ \t]*/, 'SYSTEM:\n')),

    thinking_header: $ => token(seq('##', /[ \t]*/, 'THINKING:\n')),

    tool_use_header: $ => token(seq('##', /[ \t]*/, 'TOOL', /[ \t]+/, 'USE:\n')),

    tool_result_header: $ => token(seq('##', /[ \t]*/, 'TOOL', /[ \t]+/, 'RESULT:\n')),

    server_tool_use_header: $ => token(seq('##', /[ \t]*/, 'SERVER', /[ \t]+/, 'TOOL', /[ \t]+/, 'USE:\n')),

    web_search_tool_result_header: $ => token(seq('##', /[ \t]*/, 'WEB', /[ \t]+/, 'SEARCH', /[ \t]+/, 'TOOL', /[ \t]+/, 'RESULT:\n')),

    citations_header: $ => token(seq('##', /[ \t]*/, 'CITATIONS:\n')),

    name: $ => seq(
      'Name:',
      field('value', $.value),
      /\n/
    ),

    id: $ => seq(
      'ID:',
      field('value', $.value),
      /\n/
    ),

    tool_param: $ => seq(
      '###',
      /[ ]*/,
      alias($.param_name, $.name),
      /\n/,
      optional(/\n/),
      alias($._tool_element, $.value),
    ),

    param_name: $ => /[^\n]+/,

    citation_entry: $ => seq(
      alias(token(seq('###', /[ ]*/, /https?:\/\/[^\n\s]+/)), $.url),
      /\n/,
      optional(/\n/),
      choice(
        alias($.citation_title, $.title),
        alias($.citation_text, $.cited_text),
        alias($.citation_encrypted_index, $.encrypted_index),
      ),
      optional(alias($.citation_text, $.cited_text)),
      optional(alias($.citation_encrypted_index, $.encrypted_index)),
    ),



    citation_title: $ => seq(
      'Title:',
      /[ ]+/,
      field("value", $.value),
      /\n/,
    ),

    citation_text: $ => seq(
      'Cited text:',
      /[ ]+/,
      field("value", $.value),
      /\n/,
    ),

    citation_encrypted_index: $ => seq(
      'Encrypted index:',
      /[ ]+/,
      field("value", $.value),
      /\n/,
    ),

    value: _ => /[^\n]+/,

    assistant_content_blocks: $ => prec.right(repeat1(choice(
      $.citation_entry,
      $.text,
      $.code_block,
      $.inline_code,
      $.html_comment,
      /\n/,
    ))),

    system_content_blocks: $ => prec.right(repeat1(choice(
      $.text,
      $.code_block,
      $.inline_code,
      $.html_comment,
      $.safe_shell_commands,
      /\n/,
    ))),

    content_blocks: $ => prec.right(repeat1(choice(
      $.text,
      $.code_block,
      $.inline_code,
      $.html_comment,
      /\n/,
    ))),

    text: $ => prec.right(seq(
      $._text_content,
      repeat(seq(
        /\n/,
        $._text_content,
      ))
    )),

    _text_content: $ => token(prec(-1, /[^`\n]+/)),

    _untagged_text_content: $ => token(prec(-2, seq(/[^#\n]+/, '\n'))),

    _tool_element: $ => seq(
      $.tool_start_tag,
      field('value', $.tool_content),
      $.tool_end_tag,
    ),

    content: $ => alias($._tool_element, 'content'),

    code_block: $ => seq(
      '```',
      optional(/[^\n]*/),
      /\n/,
      repeat(choice(
        /[^`\n]+/,
        /\n/,
        /`[^`]/,
        /``[^`]/,
      )),
      '```',
    ),

    inline_code: $ => seq(
      '`',
      /[^`\n]+/,
      '`',
    ),

    safe_shell_commands: $ => seq(
      '<safe-shell-commands>',
      repeat(choice(
        /[^<\n]+/,
        /\n/,
      )),
      '</safe-shell-commands>',
    ),
  }
});

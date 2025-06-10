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
  ],

  rules: {
    source_file: $ => repeat($._block),

    _block: $ => choice(
      $.user,
      $.assistant,
      $.system,
      $.thinking,
      $.tool_use,
      $.tool_result,
      $.server_tool_use,
      $.web_search_tool_result,
      $.citations,
    ),

    user: $ => seq(
      user_header,
      ':',
      $.content_blocks,
    ),

    user_header: $ => token(seq('##', /[ \t]*/, 'USER')),

    assistant: $ => seq(
      $.assistant_header,
      ':',
      $.content_blocks,
    ),

    system: $ => seq(
      $.system_header,
      ':',
      $.content_blocks,
    ),

    thinking: $ => seq(
      $.thinking_header,
      ':',
      $.content_blocks,
    ),

    tool_use: $ => seq(
      $.tool_use_header,
      ':',
      /\n/,
      optional(/\n/),
      repeat(choice(
        $.name,
        $.id,
        $.tool_param,
      )),
    ),

    tool_result: $ => seq(
      $.tool_result_header,
      ':',
      /\n/,
      optional(/\n/),
      $.id,
      optional(/\n/),
      $.content,
    ),

    server_tool_use: $ => seq(
      $.server_tool_use_header,
      ':',
      /\n/,
      optional(/\n/),
      repeat(choice(
        $.name,
        $.id,
        $.tool_param,
      )),
    ),

    web_search_tool_result: $ => seq(
      $.web_search_tool_result_header,
      ':',
      /\n/,
      optional(/\n/),
      $.id,
      optional(/\n/),
      $.content,
    ),

    citations: $ => seq(
      $.citations_header,
      ':',
      optional(alias($.citations_text, $.text)),
      repeat($.citation_entry),
    ),

    citations_text: $ => prec.right(repeat1(choice(
      $.cite_tag,
      $.safe_shell_commands,
      $._text_content,
    ))),

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
      '###',
      /[ ]*/,
      alias($.citation_url, $.url),
      /\n/,
      optional(/\n/),
      optional(alias($.citation_title, $.title)),
      optional(alias($.citation_text, $.cited_text)),
      optional(alias($.citation_encrypted_index, $.encrypted_index)),
    ),

    citation_url: $ => /[^\n]*/,

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

    content_blocks: $ => repeat1(choice(
      $.text,
      $.code_block,
      $.inline_code,
      $.html_comment,
      $.include,
      $.include_code,
    )),

    text: $ => prec.right(repeat1(choice(
      $.cite_tag,
      $.safe_shell_commands,
      $._text_content,
      /\n/,
    ))),

    _text_content: $ => token(prec(-1, /[^<`\n]+/)),

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

    cite_tag: $ => seq(
      '<cite>',
      /[^<]*/,
      '</cite>',
    ),

    safe_shell_commands: $ => seq(
      '<safe-shell-commands>',
      repeat(choice(
        /[^<\n]+/,
        /\n/,
      )),
      '</safe-shell-commands>',
    ),

    include: $ => seq(
      '<include>',
      field('path', /[^<]*/),
      '</include>',
    ),

    include_code: $ => seq(
      '<include',
      /[ \t]+/,
      'code>',
      field('path', /[^<]*/),
      '</include>',
    ),
  }
});

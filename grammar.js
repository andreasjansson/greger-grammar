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
    $.tool_content_head,
    $.tool_content_tail,
    $.html_comment,
    $.eval_content,
    $.eval_result_start_tag,
    $.eval_result_end_tag,
    $.eval_result_content_head,
    $.eval_result_content_tail,
    $.error_sentinel,
    $.code_backticks,
    $.code_language_identifier,
    $.code_contents,
  ],

  inline: $ => [
    $.content_blocks,
    $.assistant_content_blocks,
    $.system_content_blocks,
  ],

  rules: {
    source_file: $ => seq(
      optional($.untagged_text),
      optional($.system),
      repeat($._block),
    ),

    untagged_text: $ => prec(-1, repeat1(
      seq(
        $._untagged_text_content,
        "\n"
      ))),

    _block: $ => choice(
      $.user,
      $.assistant,
      $.thinking,
      $.tool_use,
      $.tool_result,
      $.server_tool_use,
      $.web_search_tool_result,
    ),

    user: $ => seq(
      $.user_header,
      '\n\n',
      $.content_blocks,
    ),

    assistant: $ => seq(
      $.assistant_header,
      '\n\n',
      $.assistant_content_blocks,
    ),

    system: $ => seq(
      $.system_header,
      '\n\n',
      $.system_content_blocks,
    ),

    thinking: $ => seq(
      $.thinking_header,
      '\n\n',
      optional(seq(
        $.thinking_signature,
        '\n\n',
      )),
      $.content_blocks,
    ),

    tool_use: $ => seq(
      $.tool_use_header,
      '\n\n',
      repeat(choice(
        $.name,
        $.id,
        $.tool_param,
      )),
    ),

    tool_result: $ => seq(
      $.tool_result_header,
      '\n\n',
      $.id,
      $.content,
    ),

    server_tool_use: $ => seq(
      $.server_tool_use_header,
      '\n\n',
      repeat(choice(
        $.name,
        $.id,
        $.tool_param,
      )),
    ),

    web_search_tool_result: $ => seq(
      $.web_search_tool_result_header,
      '\n\n',
      $.id,
      $.content,
    ),

    user_header: _ => token('# USER'),

    assistant_header: _ => token('# ASSISTANT'),
    system_header: _ => token('# SYSTEM'),

    thinking_header: _ => token('# THINKING'),

    tool_use_header: _ => token('# TOOL USE'),

    tool_result_header: _ => token('# TOOL RESULT'),

    server_tool_use_header: _ => token('# SERVER TOOL USE'),

    web_search_tool_result_header: _ => token('# WEB SEARCH TOOL RESULT'),

    name: $ => seq(
      alias('Name:', $.key),
      field('value', $.value),
      /\n/
    ),

    id: $ => seq(
      alias('ID:', $.key),
      field('value', $.value),
      /\n/
    ),

    thinking_signature: $ => seq(
      alias('Signature: ', $.key),
      field('value', $.value),
    ),

    tool_param: $ => seq(
      $.tool_param_header,
      /\n/,
      optional(/\n/),
      alias($._tool_element, $.value),
    ),

    tool_param_header: $ => seq(
      '## ',
      alias($.param_name, $.name),
    ),

    param_name: $ => /[^\n]+/,

    citation_entry: $ => seq(
      alias(token(/## https?:\/\/[^\n\s]+/), $.url),
      /\n/,
      optional(/\n/),
      seq(
        alias($.citation_title, $.title),
        alias($.citation_text, $.cited_text),
        alias($.citation_encrypted_index, $.encrypted_index),
      ),
      optional(alias($.citation_text, $.cited_text)),
      optional(alias($.citation_encrypted_index, $.encrypted_index)),
    ),



    citation_title: $ => seq(
      alias('Title: ', $.key),
      optional(field("value", $.value)),
      /\n/,
    ),

    citation_text: $ => seq(
      alias('Cited text: ', $.key),
      optional(field("value", $.value)),
      /\n/,
    ),

    citation_encrypted_index: $ => seq(
      alias('Encrypted index: ', $.key),
      field("value", $.value),
      /\n/,
    ),

    value: _ => /[^\n]+/,

    assistant_content_blocks: $ => repeat1(choice(
      $.citation_entry,
      $.code,
      alias($.assistant_text, $.text),
      $.html_comment,
    )),

    system_content_blocks: $ => repeat1(choice(
      $.html_comment,
      $.eval,
      $.code,
      $.safe_shell_commands,
      $.text,
    )),

    content_blocks: $ => repeat1(choice(
      $.html_comment,
      $.eval,
      $.code,
      $.text,
    )),

    text: $ => prec.right(repeat1(choice(
      $._text_content,
      /\n/,
    ))),

    assistant_text: $ => prec.right(repeat1(choice(
      $._assistant_text_content,
      /\n/,
    ))),

    _text_content: $ => token(prec(-1, /[^`$\n]+|\$[^{]/)),

    _assistant_text_content: $ => token(prec(-1, /[^`\n]+/)),

    _untagged_text_content: $ => token(prec(-2, seq(/[^#\n]+/, '\n'))),

    _tool_element: $ => seq(
      $.tool_start_tag,
      field('value', $.tool_content),
      $.tool_end_tag,
    ),

    tool_content: $ => seq(
      $.tool_content_head,
      optional($.tool_content_tail),
    ),

    content: $ => alias($._tool_element, 'content'),





    safe_shell_commands: $ => seq(
      '<safe-shell-commands>',
      repeat(choice(
        $.eval,
        $.shell_command,
        /\n/,
      )),
      '</safe-shell-commands>',
    ),

    // TODO: allow `<` in safe shell commands, somehow...
    shell_command: _ => token(prec(-2, /[^<\n]+/)),

    eval: $ => seq(
      $.eval_start_brace,
      repeat(choice(
        $.eval_content,
        $.eval_result,
      )),
      $.eval_end_brace,
    ),

    eval_result: $ => seq(
      $.eval_result_start_tag,
      $.eval_result_content,
      $.eval_result_end_tag,
    ),

    eval_result_content: $ => seq(
      $.eval_result_content_head,
      optional($.eval_result_content_tail),
    ),

    eval_start_brace: $ => seq(
      '${',
      optional(seq(':', $.eval_language_spec))
    ),

    eval_end_brace: $ => '}',

    eval_language_spec: $ => token(/[a-zA-Z0-9_+-]+/),

    code: $ => seq(
      $.code_backticks,
      optional(alias($.code_language_identifier, $.code_language)),
      $.code_contents,
      $.code_backticks,
    ),

  },
});

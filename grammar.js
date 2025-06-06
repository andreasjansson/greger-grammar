module.exports = grammar({
  name: 'greger',

  word: $ => $.identifier,

  extras: $ => [
    /[ \t]/
  ],

  rules: {
    document: $ => repeat($._item),

    _item: $ => choice(
      $.section,
      $.content_line,
      $.empty_line
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
      $.citations_section
    ),

    user_section: $ => '## USER:',
    system_section: $ => '## SYSTEM:',
    assistant_section: $ => '## ASSISTANT:',
    thinking_section: $ => '## THINKING:',
    citations_section: $ => '## CITATIONS:',
    tool_use_section: $ => '## TOOL USE:',
    tool_result_section: $ => '## TOOL RESULT:',
    server_tool_use_section: $ => '## SERVER TOOL USE:',
    server_tool_result_section: $ => '## SERVER TOOL RESULT:',

    content_line: $ => seq(
      choice(
        $.tool_name,
        $.tool_id,
        $.tool_parameter_header,
        $.tool_tag_open,
        $.tool_tag_close,
        $.code_fence,
        $.html_comment_start,
        $.html_comment_end,
        $.include_tag,
        $.safe_shell_commands_open,
        $.safe_shell_commands_close,
        $.text_content
      ),
      /\n/
    ),

    empty_line: $ => /\n/,

    text_content: $ => /[^\n]+/,

    tool_name: $ => prec(1, seq(
      'Name:',
      /[ \t]+/,
      $.identifier
    )),

    tool_id: $ => prec(1, seq(
      'ID:',
      /[ \t]+/,
      $.identifier
    )),

    tool_parameter_header: $ => prec(1, seq(
      '###',
      /[ \t]+/,
      $.identifier
    )),

    tool_tag_open: $ => seq(
      '<tool.',
      $.identifier,
      '>'
    ),

    tool_tag_close: $ => seq(
      '</tool.',
      $.identifier,
      '>'
    ),

    code_fence: $ => choice(
      seq('```', optional($.identifier)),
      '```'
    ),

    html_comment_start: $ => '<!--',
    html_comment_end: $ => '-->',

    include_tag: $ => seq(
      '<include',
      optional(seq(/[ \t]+/, 'code')),
      '>',
      /[^<\n]+/,
      '</include>'
    ),

    safe_shell_commands_open: $ => '<safe-shell-commands>',
    safe_shell_commands_close: $ => '</safe-shell-commands>',

    identifier: $ => /[^\s\n]+/
  }
});

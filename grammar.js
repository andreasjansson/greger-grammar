module.exports = grammar({
  name: 'greger',

  extras: $ => [
    /[ \t\r]/
  ],

  rules: {
    document: $ => seq(
      optional($.untagged_content),
      repeat($.section)
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

    user_section: $ => seq(
      '## USER:',
      optional(/\n/),
      optional($.content)
    ),

    system_section: $ => seq(
      '## SYSTEM:',
      optional(/\n/),
      optional($.content)
    ),

    assistant_section: $ => seq(
      '## ASSISTANT:',
      optional(/\n/),
      optional($.content)
    ),

    thinking_section: $ => seq(
      '## THINKING:',
      optional(/\n/),
      optional($.content)
    ),

    citations_section: $ => seq(
      '## CITATIONS:',
      optional(/\n/),
      optional($.content)
    ),

    tool_use_section: $ => seq(
      '## TOOL USE:',
      optional(/\n/),
      optional($.tool_use_content)
    ),

    tool_result_section: $ => seq(
      '## TOOL RESULT:',
      optional(/\n/),
      optional($.tool_result_content)
    ),

    server_tool_use_section: $ => seq(
      '## SERVER TOOL USE:',
      optional(/\n/),
      optional($.tool_use_content)
    ),

    server_tool_result_section: $ => seq(
      '## SERVER TOOL RESULT:',
      optional(/\n/),
      optional($.tool_result_content)
    ),

    content: $ => repeat1(choice(
      $.text_line,
      $.empty_line,
      $.code_block,
      $.html_comment,
      $.include_tag,
      $.safe_shell_commands_tag
    )),

    tool_use_content: $ => repeat1(choice(
      $.tool_name_line,
      $.tool_id_line,
      $.tool_parameter,
      $.text_line,
      $.empty_line
    )),

    tool_result_content: $ => repeat1(choice(
      $.tool_id_line,
      $.tool_output,
      $.text_line,
      $.empty_line
    )),

    untagged_content: $ => repeat1(choice(
      $.text_line,
      $.empty_line
    )),

    text_line: $ => seq(
      /[^\n#]+/,
      /\n/
    ),

    empty_line: $ => /\n/,

    tool_name_line: $ => seq(
      'Name:',
      /[ \t]+/,
      $.identifier,
      /\n/
    ),

    tool_id_line: $ => seq(
      'ID:',
      /[ \t]+/,
      $.identifier,
      /\n/
    ),

    tool_parameter: $ => seq(
      '###',
      /[ \t]+/,
      $.identifier,
      /\n/,
      optional($.parameter_value)
    ),

    parameter_value: $ => seq(
      '<tool.',
      $.identifier,
      '>',
      /\n/,
      repeat(choice(
        $.parameter_line,
        $.empty_line
      )),
      '</tool.',
      $.identifier,
      '>',
      /\n/
    ),

    parameter_line: $ => seq(
      /[^\n<]+/,
      /\n/
    ),

    tool_output: $ => seq(
      '<tool.',
      $.identifier,
      '>',
      /\n/,
      repeat(choice(
        $.output_line,
        $.empty_line
      )),
      '</tool.',
      $.identifier,
      '>',
      /\n/
    ),

    output_line: $ => seq(
      /[^\n<]+/,
      /\n/
    ),

    code_block: $ => seq(
      '```',
      optional($.identifier),
      /\n/,
      repeat(choice(
        $.code_line,
        $.empty_line
      )),
      '```',
      /\n/
    ),

    code_line: $ => seq(
      /[^\n]+/,
      /\n/
    ),

    html_comment: $ => seq(
      '<!--',
      repeat(/[^>]/),
      '-->',
      /\n/
    ),

    include_tag: $ => seq(
      '<include',
      optional(seq(/[ \t]+/, 'code')),
      '>',
      /[^<\n]+/,
      '</include>',
      /\n/
    ),

    safe_shell_commands_tag: $ => seq(
      '<safe-shell-commands>',
      /\n/,
      repeat(seq(
        /[^\n<]+/,
        /\n/
      )),
      '</safe-shell-commands>',
      /\n/
    ),

    identifier: $ => /[^\s\n]+/
  }
});

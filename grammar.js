module.exports = grammar({
  name: 'greger',

  extras: $ => [
    /[ \t\r]/
  ],

  rules: {
    document: $ => repeat(choice(
      $.section_header,
      $.content_line,
      $.empty_line
    )),

    section_header: $ => choice(
      '## USER:',
      '## SYSTEM:',
      '## ASSISTANT:',
      '## THINKING:',
      '## TOOL USE:',
      '## TOOL RESULT:',
      '## SERVER TOOL USE:',
      '## SERVER TOOL RESULT:',
      '## CITATIONS:'
    ),

    content_line: $ => seq(
      choice(
        $.tool_name_line,
        $.tool_id_line,
        $.tool_parameter_header,
        $.tool_tag,
        $.code_fence,
        $.html_comment_marker,
        $.include_tag,
        $.safe_shell_tag,
        $.cite_tag,
        $.text_content
      ),
      /\n/
    ),

    empty_line: $ => /\n/,

    tool_name_line: $ => seq(
      'Name:',
      /[ \t]+/,
      /[^\s\n]+/
    ),

    tool_id_line: $ => seq(
      'ID:',
      /[ \t]+/,
      /[^\s\n]+/
    ),

    tool_parameter_header: $ => seq(
      '###',
      /[ \t]+/,
      /[^\s\n]+/
    ),

    tool_tag: $ => choice(
      seq('<tool.', /[^\s>]+/, '>'),
      seq('</tool.', /[^\s>]+/, '>')
    ),

    code_fence: $ => seq(
      '```',
      optional(/[^\n]*/)
    ),

    html_comment_marker: $ => choice(
      '<!--',
      '-->'
    ),

    include_tag: $ => seq(
      '<include',
      optional(seq(/[ \t]+/, 'code')),
      '>',
      /[^<\n]+/,
      '</include>'
    ),

    safe_shell_tag: $ => choice(
      '<safe-shell-commands>',
      '</safe-shell-commands>'
    ),

    cite_tag: $ => choice(
      '<cite>',
      '</cite>'
    ),

    text_content: $ => /[^\n]+/
  }
});

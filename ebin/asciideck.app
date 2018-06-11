{application, 'asciideck', [
	{description, "Asciidoc for Erlang."},
	{vsn, "0.2.0"},
	{modules, ['asciideck','asciideck_attributes_parser','asciideck_attributes_pass','asciideck_block_parser','asciideck_inline_pass','asciideck_line_reader','asciideck_lists_pass','asciideck_reader','asciideck_stdin_reader','asciideck_tables_pass','asciideck_to_html','asciideck_to_manpage']},
	{registered, []},
	{applications, [kernel,stdlib]},
	{env, []}
]}.
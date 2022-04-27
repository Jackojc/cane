# cane

provide-module -override cane %{
	add-highlighter shared/cane regions
	add-highlighter shared/cane/other default-region group

	# comment
	add-highlighter shared/cane/comment region '#' '$' group
	add-highlighter shared/cane/comment/ fill comment

	# literals
	add-highlighter shared/cane/other/ regex "\b\d+" 0:value             # decimal
	add-highlighter shared/cane/other/ regex "\b0[xX][a-fA-F0-9]+" 0:value # hex
	add-highlighter shared/cane/other/ regex "\b0[bB][0-1]+" 0:value       # binary
	add-highlighter shared/cane/other/ regex "!|\." 0:value              # steps

	# keywords and operators
	add-highlighter shared/cane/other/ regex "\b(midi)\b" 0:keyword
	add-highlighter shared/cane/other/ regex "~>|=>|@|/|\+|\||&|\^|,|~|<<|>>|<|>|\*" 0:operator
}

hook global BufCreate .*\.(cn) %{ set-option buffer filetype cane }
hook global WinSetOption filetype=cane %{ require-module cane }

hook -group cane-highlight global WinSetOption filetype=cane %{
	add-highlighter window/cane ref cane
	hook -once -always window WinSetOption filetype=.* %{ remove-highlighter window/cane }
}

# comment token
hook global BufSetOption filetype=cane %{
	set-option buffer comment_line '#'
	set-option buffer comment_block_begin '#'
	set-option buffer comment_block_end ''
}


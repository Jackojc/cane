#include <iostream>
#include <string>

#include <report.hpp>
#include <view.hpp>
#include <lib.hpp>

int main(int, const char*[]) {
	try {
		std::istreambuf_iterator<char> begin(std::cin), end;
		std::string in(begin, end);

		cane::View src { &*in.begin(), &*in.end() };
		cane::Lexer lx { src };

		if (not cane::utf_validate(src))
			lx.error(cane::Phases::PHASE_ENCODING, src, cane::STR_ENCODING);

		// while (lx.peek().kind != cane::Symbols::TERMINATOR) {
		// 	printlnfmt("{} => {}", lx.peek().kind, lx.peek().view);
		// 	lx.next();
		// }

		cane::Instructions is = cane::compile(lx);
	}

	catch (cane::Error) {
		return 1;
	}

	return 0;
}

#include <iostream>
#include <string>
#include <chrono>

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

		namespace time = std::chrono;

		using clock = time::steady_clock;
		using unit = time::microseconds;

		auto t1 = clock::now();
		cane::Context ctx = cane::compile(lx);
		auto t2 = clock::now();

		time::duration<double, std::micro> t = t2 - t1;
		cane::printlnfmt(CANE_ANSI_FG_YELLOW "took: {}µs" CANE_ANSI_RESET, t.count());

		// Ugly debug output
		int i = 0;
		for (cane::Chain& c: ctx.chains) {
			if (c.empty()) continue;
			cane::print(CANE_ANSI_BOLD CANE_ANSI_FG_BRIGHT_YELLOW "midi", i, CANE_ANSI_RESET " ");

			for (cane::Pattern& p: c) {
				cane::print(CANE_ANSI_BOLD "[" CANE_ANSI_RESET);

				for (bool s: p.seq) {
					cane::print(s ?
						CANE_ANSI_BOLD CANE_ANSI_FG_BRIGHT_YELLOW "!" CANE_ANSI_RESET :
						CANE_ANSI_FG_BLUE "." CANE_ANSI_RESET
					);
				}

				cane::print(CANE_ANSI_BOLD "]" CANE_ANSI_RESET " ");
			}

			cane::println();
			i++;
		}
	}

	catch (cane::Error) {
		return 1;
	}

	return 0;
}

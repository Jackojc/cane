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

		std::stable_sort(ctx.channels.begin(), ctx.channels.end(), [] (const auto& a, const auto& b) {
			return a.channel < b.channel;
		});


		for (auto it = ctx.channels.begin(); it != ctx.channels.end();) {
			size_t channel = it->channel;

			cane::print(CANE_ANSI_BOLD CANE_ANSI_FG_BRIGHT_YELLOW "midi", channel, CANE_ANSI_RESET " ");

			for (; it != ctx.channels.end(); ++it) {
				if (it->channel != channel)
					break;

				cane::print(it->seq);
			}

			cane::println();
		}
	}

	catch (cane::Error) {
		return 1;
	}

	return 0;
}

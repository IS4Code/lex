// MIT License
//
// Copyright (c) 2020 PG1003
// Copyright (C) 1994-2020 Lua.org, PUC-Rio.
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

#pragma once

#include <cassert>

#include <string>
#if defined(__cpp_lib_string_view)
#include <string_view>
#endif
#include <stdexcept>
#include <algorithm>
#include <utility>
#include <type_traits>
#include <memory>
#include <iterator>
#include <tuple>
#include <cctype>
#include <array>

#ifdef __has_cpp_attribute
# if __has_cpp_attribute(unlikely)
#  define PG_LEX_UNLIKELY [[unlikely]]
# endif
# if __has_cpp_attribute(nodiscard)
#  define PG_LEX_NODISCARD [[nodiscard]]
# endif
# if __has_cpp_attribute(fallthrough)
#  define PG_LEX_FALLTHROUGH [[fallthrough]]
# endif
#endif
#ifndef PG_LEX_UNLIKELY
# define PG_LEX_UNLIKELY
#endif
#ifndef PG_LEX_NODISCARD
# define PG_LEX_NODISCARD
#endif
#ifndef PG_LEX_FALLTHROUGH
# define PG_LEX_FALLTHROUGH
#endif

// Maximum recursion depth for 'match'
#if !defined(MAXCCALLS)
#define MAXCCALLS 200
#endif

// Maximum number of captures that a pattern can do during pattern-matching.
#if !defined(MAXCAPTURES)
#define MAXCAPTURES 32
#endif

namespace pg
{
	namespace lex
	{
		namespace detail
		{
			template <typename T>
			struct string_traits
			{
				using char_type = typename std::iterator_traits<decltype(std::begin(std::declval<typename std::remove_cv<T>::type>()))>::value_type;
			};

			template <typename T>
			struct string_traits<T*>
			{
				using char_type = typename std::remove_cv<T>::type;
			};

			template <typename T>
			struct string_traits<T* const>
			{
				using char_type = typename std::remove_cv<T>::type;
			};

			template <typename T>
			struct string_traits<T[]> : string_traits<T*> {};

			template <typename T>
			struct string_traits<T&> : string_traits<T> {};

			template <typename T>
			struct string_traits<T&&> : string_traits<T> {};

			template <typename T>
			using char_type = typename string_traits<T>::char_type;

			template <typename, typename>
			struct match_state_iter;

			template <typename Iter>
			struct capture_iter : public std::pair<Iter, Iter>
			{
				using iterator = Iter;
				using value_type = typename std::iterator_traits<Iter>::value_type;
				using difference_type = typename std::iterator_traits<Iter>::difference_type;
				using string_type = std::basic_string<value_type>;

				Iter init() const noexcept(std::is_nothrow_move_constructible<Iter>::value)
				{
					assert(matched);
					return first;
				}

				void init(Iter i) noexcept(std::is_nothrow_copy_assignable<Iter>::value)
				{
					first = i;
				}

				difference_type len() const
				{
					return std::distance(first, second);
				}

				void len(difference_type l)
				{
					assert(l >= 0);
					second = std::next(first, l);
				}

				void mark_unfinished() noexcept
				{
					matched = false;
				}

				void mark_position() noexcept
				{
					matched = true;
				}

				bool is_unfinished() const noexcept
				{
					return matched;
				}

				Iter begin() const noexcept(std::is_nothrow_move_constructible<Iter>::value)
				{
					return first;
				}

				Iter end() const noexcept(std::is_nothrow_move_constructible<Iter>::value)
				{
					return second;
				}

#if defined(PG_LEX_TESTS)
				operator string_type() const
				{
					assert(!is_unfinished());
					return {first, second};
				}

				bool operator==(const string_type &str) const
				{
					return static_cast<string_type>(*this) == str;
				}

				Iter data() const noexcept { return init(); }
				std::size_t size() const noexcept { return len(); }
#endif

				bool matched = false;
			};

			template <typename Iter>
			struct captures_iter
			{
				using char_type = typename std::iterator_traits<Iter>::value_type;
				using string_type = typename std::basic_string<char_type>;

				captures_iter() noexcept = default;

				captures_iter(const captures_iter &other) noexcept(std::is_nothrow_copy_assignable<Iter>::value)
				{
					operator=(other);
				}

				captures_iter(captures_iter &&other) noexcept(std::is_nothrow_move_constructible<Iter>::value)
					: local(std::move(other.local))
					, alloc(std::move(other.alloc))
				{}

				captures_iter &operator=(const captures_iter &other) noexcept(std::is_nothrow_copy_assignable<Iter>::value)
				{
					if(other.alloc)
					{
						alloc = std::make_unique<std::array<capture_iter<Iter>, MAXCAPTURES>>(*other.alloc);
						local = {};
					}
					else
					{
						alloc = {};
						local = other.local;
					}

					return *this;
				}

				captures_iter &operator=(captures_iter &&other) noexcept(std::is_nothrow_move_assignable<Iter>::value)
				{
					if(other.alloc)
					{
						alloc = std::move(other.alloc);
						local = {};
					}
					else
					{
						alloc = {};
						local = std::move(other.local);
					}

					return *this;
				}

				capture_iter<Iter> &operator[](std::size_t idx) noexcept
				{
					if(alloc)
					{
						return (*alloc)[idx];
					}
					else if(idx >= max_local)
					{
						alloc = std::make_unique<std::array<capture_iter<Iter>, MAXCAPTURES>>();
						std::copy(local.begin(), local.end(), alloc->begin());
						return (*alloc)[idx];
					}

					return local[idx];
				}

				const capture_iter<Iter> &operator[](std::size_t idx) const noexcept
				{
					return alloc ? (*alloc)[idx] : local[idx];
				}

				const capture_iter<Iter> *data() const noexcept
				{
					return &(*this)[0];
				}

			private:
				static constexpr int max_local = 2;
				std::array<capture_iter<Iter>, max_local> local;
				std::unique_ptr<std::array<capture_iter<Iter>, MAXCAPTURES>> alloc;

				static_assert(MAXCAPTURES > max_local, "MAXCAPTURES must be greater than 2");
			};
		}

		template <typename>
		struct basic_match_result_iter;

		enum error_type : int
		{
			pattern_too_complex,
			pattern_ends_with_percent,
			pattern_missing_closing_bracket,
			balanced_no_arguments,
			frontier_no_open_bracket,
			capture_too_many,
			capture_invalid_pattern,
			capture_invalid_index,
			capture_not_finished,
			capture_out_of_range,
			percent_invalid_use_in_replacement
		};

		class lex_error : public std::runtime_error
		{
			const error_type error_code;

		public:
			lex_error(const std::string&) = delete;
			lex_error(const char*) = delete;

			lex_error(pg::lex::error_type ec) noexcept;

			PG_LEX_NODISCARD error_type code() const noexcept
			{
				return error_code;
			}
		};

		/**
		 * \brief The base template for a match result
		 */
		template <typename Iter>
		struct basic_match_result_iter
		{
			using value_type = detail::capture_iter<Iter>;
			using const_reference = const value_type&;
			using reference = value_type&;
			using const_iterator = const detail::capture_iter<Iter>*;
			using iterator = const_iterator;
			using difference_type = typename std::iterator_traits<Iter>::difference_type;
			using char_type = typename std::iterator_traits<Iter>::value_type;
			using string_type = typename std::basic_string<char_type>;

		private:
			template <typename, typename>
			friend struct detail::match_state_iter;

			std::pair<difference_type, difference_type> pos = { -1, -1 }; // The indices where the match starts and ends
			int level = 0; // Total number of captures (finished or unfinished)
			detail::captures_iter<Iter> captures;

		public:
			/**
			 * \brief Returns an iterator to the begin of the capture list.
			 */
			PG_LEX_NODISCARD iterator begin() const noexcept(std::is_nothrow_move_constructible<iterator>::value)
			{
				return captures.data();
			}

			/**
			 * \brief Returns an iterator to the end of the capture list.
			 */
			PG_LEX_NODISCARD iterator end() const noexcept(std::is_nothrow_move_constructible<iterator>::value)
			{
				return captures.data() + size();
			}

			/**
			 * \brief Returns the number of captures.
			 */
			PG_LEX_NODISCARD size_t size() const noexcept
			{
				assert(level >= 0);
				return level;
			}

			/**
			 * \brief Conversion to bool for easy evaluation if the match result contains match data.
			 */
			PG_LEX_NODISCARD operator bool() const noexcept
			{
				return level > 0;
			}

			/**
			 * \brief Returns a std::string_view of the requested capture.
			 *
			 * This function throws a 'capture_out_of_range' when match result doesn't have a capture at the requested index.
			 */
			PG_LEX_NODISCARD const_reference at(size_t i) const
			{
				if(static_cast<int>(i) >= level) PG_LEX_UNLIKELY
				{
					throw lex_error(capture_out_of_range);
				}

				return captures[i];
			}

			/**
			 * \brief Returns a pair of indices that tells the position of the match in the string.
			 *
			 * First is the start index of the match and second one past the last character of the match.
			 *
			 * \note first and second are -1 when the match result doesn't contain match data.
			 */
			PG_LEX_NODISCARD const std::pair<difference_type, difference_type> &position() const noexcept
			{
				return pos;
			}

			/**
			 * \brief Returns the length of the match.
			 */
			PG_LEX_NODISCARD size_t length() const noexcept
			{
				return pos.second - pos.first;
			}
		};

		template <class CharT>
		using basic_match_result = basic_match_result_iter<const CharT*>;

		using match_result = basic_match_result<char>;
		using wmatch_result = basic_match_result<wchar_t>;
#if defined(__cpp_lib_char8_t)
		using u8match_result = basic_match_result<char8_t>;
#endif
		using u16match_result = basic_match_result<char16_t>;
		using u32match_result = basic_match_result<char32_t>;

		namespace detail
		{
			template <typename Iter>
			using pos_result_iter = std::pair<Iter, bool>;

			template <typename CharT>
			using pos_result = pos_result_iter<const CharT*>;

			template <typename StrIter, typename PatIter>
			using common_unsigned_char_iter = typename std::make_unsigned<typename std::common_type<typename std::iterator_traits<StrIter>::value_type, typename std::iterator_traits<PatIter>::value_type>::type>;

			template <typename StrCharT, typename PatCharT>
			using common_unsigned_char = common_unsigned_char_iter<const StrCharT*, const PatCharT*>;

			template <typename StrIter, typename PatIter>
			struct match_state_iter
			{
				using difference_type = typename std::iterator_traits<StrIter>::difference_type;

				match_state_iter(StrIter str_begin, StrIter str_end, PatIter pat_end, basic_match_result_iter<StrIter> &mr) noexcept(std::is_nothrow_move_constructible<StrIter>::value && std::is_nothrow_move_constructible<PatIter>::value)
					: s_begin(str_begin)
					, s_end(str_end)
					, p_end(pat_end)
					, level(mr.level)
					, captures(mr.captures)
					, pos(mr.pos)
				{
					reprepstate();
				}

				void reprepstate() noexcept
				{
					assert(matchdepth == MAXCCALLS);

					level = 0;
					pos = { -1, -1 };
				}

				StrIter const s_begin;
				StrIter const s_end;
				PatIter const p_end;
				int matchdepth = MAXCCALLS; // Control for recursive depth (to avoid stack overflow)

				int &level; // Total number of captures (finished or unfinished)
				detail::captures_iter<StrIter> &captures;
				std::pair<difference_type, difference_type> &pos;
			};

			template <typename StrCharT, typename PatCharT>
			using match_state = match_state_iter<const StrCharT*, const PatCharT*>;

			template <typename StrIter, typename PatIter>
			pos_result_iter<StrIter> match(match_state_iter<StrIter, PatIter> &ms, StrIter s, PatIter p);

			bool match_class(int c, int cl) noexcept;

			template <typename PatIter>
			PatIter find_bracket_class_end(PatIter p)
			{
				do
				{
					if(*p == '%')
					{
						std::advance(p, 2);
					}
				}
				while(*p++ != ']');

				return p;
			}

			template <typename StrIter, typename PatIter>
			pos_result_iter<PatIter> matchbracketclass(const match_state_iter<StrIter, PatIter> &ms, const typename std::iterator_traits<StrIter>::value_type c, PatIter p)
			{
				using uchar_t = typename common_unsigned_char_iter<StrIter, PatIter>::type;

				const uchar_t uc = static_cast<uchar_t>(c);

				bool ret = true;
				if(*(++p) == '^')
				{
					ret = false;
					++p; // Skip the '^'
				}

				do
				{
					if(*p == '%')
					{
						++p; // Skip escapes (e.g. '%]')
						if(match_class(uc, *p))
						{
							return {std::next(p), ret};
						}
					}
					else
					{
						PatIter ec = std::next(p, 2);
						if(*std::next(p) == '-' && ec != ms.p_end && *ec != ']')
						{
							const auto min = static_cast<uchar_t>(*p);
							const auto max = static_cast<uchar_t>(*ec);
							if(min <= uc && uc <= max)
							{
								return {std::next(ec), ret};
							}
							p = ec;
						}
						else if(static_cast<uchar_t>(*p) == uc)
						{
							return {std::next(p), ret};
						}
					}

					++p;
				}
				while(*p != ']');

				return {p, !ret};
			}

			template <typename StrIter, typename PatIter>
			pos_result_iter<PatIter> single_match_pr(const match_state_iter<StrIter, PatIter> &ms, StrIter s, PatIter p)
			{
				using uchar_t = typename common_unsigned_char_iter<StrIter, PatIter>::type;

				const bool not_end = s != ms.s_end;

				switch(*p)
				{
					case '.':
						return {std::next(p), not_end}; // Matches any char

					case '%':
						return {std::next(p, 2), not_end && match_class(static_cast<uchar_t>(*s), *std::next(p))};

					case '[':
						if(not_end)
						{
							PatIter ep;
							bool res;
							std::tie(ep, res) = matchbracketclass(ms, *s, p);
							return {find_bracket_class_end(ep), res};
						}
						return {find_bracket_class_end(p), false};

					default:
						return {std::next(p), not_end && static_cast<uchar_t>(*s) == static_cast<uchar_t>(*p)};
				}
			}

			template <typename StrIter, typename PatIter>
			bool single_match(const match_state_iter<StrIter, PatIter> &ms, const typename std::iterator_traits<StrIter>::value_type c, PatIter p)
			{
				using uchar_t = typename common_unsigned_char_iter<StrIter, PatIter>::type;

				switch(*p)
				{
					case '.':
						return true; // Matches any char

					case '%':
						return match_class(static_cast<uchar_t>(c), *std::next(p));

					case '[':
						return matchbracketclass(ms, c, p).second;

					default:
						return static_cast<uchar_t>(*p) == static_cast<uchar_t>(c);
				}
			}

			template <typename StrIter, typename PatIter>
			StrIter matchbalance(const match_state_iter<StrIter, PatIter> &ms, StrIter s, PatIter p)
			{
				using uchar_t = typename common_unsigned_char_iter<StrIter, PatIter>::type;

				if(static_cast<uchar_t>(*s) != static_cast<uchar_t>(*p))
				{
					return ms.s_end;
				}
				else
				{
					const auto b = static_cast<uchar_t>(*p);
					const auto e = static_cast<uchar_t>(*std::next(p));
					int count = 1;
					while(++s != ms.s_end)
					{
						const auto uc = static_cast<uchar_t>(*s);
						if(uc == e)
						{
							if(--count == 0)
							{
								return s;
							}
						}
						else if(uc == b)
						{
						  ++count;
						}
					}
				}
				return ms.s_end;
			}

			template <typename StrIter, typename PatIter>
			pos_result_iter<StrIter> max_expand(match_state_iter<StrIter, PatIter> &ms, StrIter s, PatIter p, PatIter ep)
			{
				ptrdiff_t i = 0;
				while(std::next(s, i) != ms.s_end && single_match(ms, *std::next(s, i), p))
				{
					++i;
				}
				// Keeps trying to match with the maximum repetitions
				while(i >= 0)
				{
					auto res = match(ms, std::next(s, i), std::next(ep));
					if(res.second)
					{
						return res;
					}
					--i;
				}

				return {s, false};
			}

			template <typename StrIter, typename PatIter>
			pos_result_iter<StrIter> min_expand(match_state_iter<StrIter, PatIter> &ms, StrIter s, PatIter p, PatIter ep)
			{
				for(; ;)
				{
					auto res = match(ms, s, std::next(ep));
					if(res.second)
					{
						return res;
					}
					else if(s != ms.s_end && single_match(ms, *s, p))
					{
						++s;
					}
					else
					{
						return {s, false};
					}
				}
			}

			template <typename StrIter, typename PatIter>
			pos_result_iter<StrIter> start_capture(match_state_iter<StrIter, PatIter> &ms, StrIter s, PatIter p)
			{
				ms.captures[ms.level].init(s);

				if(*p == ')')
				{
					ms.captures[ms.level].mark_position();
					++p;
				}
				else
				{
					ms.captures[ms.level].mark_unfinished();
				}

				++ms.level;

				auto res = match(ms, s, p);
				if(!res.second)
				{
					// Undo capture when the match has failed
					--ms.level;
					ms.captures[ms.level].mark_unfinished();
				}
				return res;
			}

			template <typename StrIter, typename PatIter>
			pos_result_iter<StrIter> end_capture(match_state_iter<StrIter, PatIter> &ms, StrIter s, PatIter p)
			{
				int i = ms.level;
				for(--i ; i >= 0 ; --i)
				{
					detail::capture_iter<StrIter> &cap = ms.captures[i];
					if(cap.is_unfinished())
					{
						cap.len(static_cast<int>(std::distance(cap.init(), s)));

						auto res = match(ms, s, p);
						if(!res.second)
						{
							// Undo capture when the match has failed
							cap.mark_unfinished();
						}
						return res;
					}
				}

				return {s, false};
			}

			template <typename StrIter, typename PatIter>
			pos_result_iter<StrIter> match_capture(match_state_iter<StrIter, PatIter> &ms, StrIter s, typename std::iterator_traits<PatIter>::value_type c)
			{
				const int i = c - '1';
				const int len = ms.captures[i].len();
				StrIter c_begin = ms.captures[i].init();
				StrIter c_end = std::next(c_begin, len);
				if(std::distance(s, ms.s_end) >= len &&
					std::equal(c_begin, c_end, s))
				{
					return {std::next(s, len), true};
				}

				return {s, false};
			}

			template <typename StrIter, typename PatIter>
			pos_result_iter<StrIter> match(match_state_iter<StrIter, PatIter> &ms, StrIter s, PatIter p)
			{
				using StrCharT = typename std::iterator_traits<StrIter>::value_type;

				while(p != ms.p_end)
				{
					switch(*p)
					{
						case '(': // Start capture
							return start_capture(ms, s, std::next(p));

						case ')': // End capture
							return end_capture(ms, s, std::next(p));

						case '$':
							if(std::next(p) != ms.p_end) // Is the '$' the last char in pattern?
							{
								break;
							}
							return {s, s == ms.s_end};

						case '%': // Escaped sequences not in the format class[*+?-]?
							switch(*std::next(p))
							{
								case 'b': // Balanced string?
								{
									auto res = matchbalance(ms, s, std::next(p, 2));
									if(res != ms.s_end)
									{
										s = std::next(res);
										std::advance(p, 4);
										continue;
									}
									return {s, false};
								}

								case 'f': // Frontier?
									std::advance(p, 2);
									if(matchbracketclass(ms, *s, p).second)
									{
										const StrCharT previous = (s == ms.s_begin) ? '\0' : *std::prev(s);
										PatIter ep;
										bool res;
										std::tie(ep, res) = matchbracketclass(ms, previous, p) ;
										if(!res)
										{
											assert(*ep == ']');
											p = std::next(ep);
											continue;
										}
									}
									return {s, false};

								case '0': case '1': case '2': case '3': case '4':
								case '5': case '6': case '7': case '8': case '9': // Capture results (%0-%9)?
								{
									auto res = match_capture(ms, s, *std::next(p));
									if(res.second)
									{
										s = res.first;
										std::advance(p, 2);
										continue;
									}
									return {s, false};
								}
							}
					}

					PatIter ep;
					bool r;
					std::tie(ep, r) = single_match_pr(ms, s, p);
					if(r)
					{
						switch(*ep) // Handle optional suffix
						{
							case '+': // 1 or more repetitions
								++s; // 1 match already done
								PG_LEX_FALLTHROUGH;
							case '*': // 0 or more repetitions
								return max_expand(ms, s, p, ep);

							case '-': // 0 or more repetitions (minimum)
								return min_expand(ms, s, p, ep);

							default: // No suffix
							{
								auto res = match(ms, std::next(s), ep);
								if(res.second)
								{
									return res;
								}
								return {s, false};
							}

							case '?': // Optional
							{
								auto res = match(ms, std::next(s), std::next(ep));
								if(res.second)
								{
									return res;
								}
							}
						}
					}
					else if(*ep != '*' && *ep != '?' && *ep != '-') // Accept empty?
					{
						return {s, false};
					}

					p = std::next(ep);
				}

				return {s, true};
			}

			template <typename CharT>
			void append_number(std::basic_string<CharT>& str, ptrdiff_t number) noexcept
			{
				assert(number >= 0);

				if(number > 9)
				{
					append_number(str, number / 10);
				}
				str.append(1, '0' + (number % 10));
			}

			template <typename Iter>
			struct string_context_iter
			{
				string_context_iter(const Iter begin, const Iter end) noexcept(std::is_nothrow_copy_constructible<Iter>())
					: begin(begin), end(end)
				{

				}

				const Iter begin;
				const Iter end;
			};

			template <typename CharT>
			struct string_context : string_context_iter<const CharT*>
			{
				string_context(const CharT *s) noexcept
					: string_context_iter(s, s + std::char_traits<CharT>::length(s))
				{
				}

				string_context(const CharT *s, size_t l) noexcept
					: string_context_iter(s, s + l)
				{
				}

				template <size_t N>
				string_context(const CharT(&s)[N]) noexcept
					: string_context_iter(std::begin(s), std::end(s))
				{
				}

				template <typename Traits, typename Allocator>
				string_context(const std::basic_string<CharT, Traits, Allocator> &s) noexcept
					: string_context_iter(&s[0], &s[0] + s.size())
				{
				}

#if defined(__cpp_lib_string_view)
				template <typename Traits>
				string_context(const std::basic_string_view<CharT, Traits> &s) noexcept
					: string_context_iter(&s[0], &s[0] + s.size())
				{
				}
#endif
			};

			template <typename Iter>
			auto find_bracket_class_end(Iter p, Iter ep)
			{
				do
				{
					if(p == ep) PG_LEX_UNLIKELY
					{
						throw lex_error(pattern_missing_closing_bracket);
					}
					else if(*p == '%')
					{
						if(++p == ep) PG_LEX_UNLIKELY // Skip escapes (e.g. '%]')
						{
							throw lex_error(pattern_ends_with_percent);
						}
						if(++p == ep) PG_LEX_UNLIKELY
						{
							throw lex_error(pattern_missing_closing_bracket);
						}
					}
				}
				while(*p++ != ']');

				return p;
			}
		}

		template <typename Iter>
		struct pattern_iter
		{
			pattern_iter(const Iter begin, const Iter end)
				: end(end)
				, anchor(begin != end && *begin == '^')
				, begin(anchor ? std::next(begin) : begin)
			{
				enum class capture_state { available, unfinished, finished };

				int depth = 0;
				capture_state captures[MAXCAPTURES] = {};
				int capture_level = 0;

				auto q = begin;
				while(q != end)
				{
					switch(*q)
					{
						case '(':
							if(capture_level > MAXCAPTURES) PG_LEX_UNLIKELY
							{
								throw lex_error(capture_too_many);
							}
							captures[capture_level++] = capture_state::unfinished;
							++q;
							++depth;
							continue;

						case ')':
						{
							auto level = capture_level;
							while(--level >= 0)
							{
								if(captures[level] == capture_state::unfinished)
								{
									captures[level] = capture_state::finished;
									break;
								}
							}
							if(level < 0) PG_LEX_UNLIKELY
							{
								throw lex_error(capture_invalid_pattern);
							}
							++q;
							++depth;
							continue;
						}

						case '$':
							++q;
							continue;

						case '%':
							if(++q == end) PG_LEX_UNLIKELY
							{
								throw lex_error(pattern_ends_with_percent);
							}
							switch(*q)
							{
							case 'b':
								if(std::distance(q, end) < 3) PG_LEX_UNLIKELY
								{
									throw lex_error(balanced_no_arguments);
								}
								std::advance(q, 3);
								continue;

							case 'f':
								if(++q == end || *q != '[') PG_LEX_UNLIKELY
								{
									throw lex_error(frontier_no_open_bracket);
								}
								q = detail::find_bracket_class_end(q, end);
								continue;

							case '0': case '1': case '2': case '3': case '4':
							case '5': case '6': case '7': case '8': case '9':
								const int i = *q - '1';
								if(i < 0 ||
									i >= capture_level ||
									captures[i] != capture_state::finished) PG_LEX_UNLIKELY
								{
									throw lex_error(capture_invalid_index);
								}
								++q;
								continue;
							}
					}

					if(q != end && *q == '[')
					{
						q = detail::find_bracket_class_end(q, end);
					}
					else
					{
						++q;
					}
					if(q != end && (*q == '*' || *q == '+' || *q == '?' || *q == '-'))
					{
						++q;
					}

					++depth;
				}

				if(std::any_of(captures, captures + capture_level,
								 [](const auto cap){ return cap != capture_state::finished; })) PG_LEX_UNLIKELY
				{
					throw lex_error(capture_not_finished);
				}

				if(depth > MAXCCALLS) PG_LEX_UNLIKELY
				{
					throw lex_error(pattern_too_complex);
				}
			}

			const Iter end;
			const bool anchor;
			const Iter begin;
		};

		template <class CharT>
		struct pattern : public pattern_iter<const CharT*>
		{
			pattern(const CharT *p) : pattern_iter(p, p + std::char_traits<CharT>::length(p))
			{
			}

			pattern(const CharT *p, size_t l) : pattern_iter(p, p + l)
			{
			}

			template <size_t N>
			pattern(const CharT(&p)[N]) : pattern_iter(std::begin(p), std::end(p))
			{
			}

			template <typename Traits, typename Allocator>
			pattern(const std::basic_string<CharT, Traits, Allocator> &s) : pattern_iter(&s[0], &s[0] + s.size())
			{
			}

#if defined(__cpp_lib_string_view)
			template <typename Traits>
			pattern(const std::basic_string_view<CharT, Traits> &s) : pattern_iter(&s[0], &s[0] + s.size())
			{
			}
#endif
		};

		/**
		 * \brief A lex gmatch_context is an input string combined with a pattern.
		 *
		 * You can get a pg::lex::gmatch_iterator using the pg::lex::begin and pg::lex::end functions and
		 * iterate with it over all matches in a lex context.
		 *
		 * A lex gmatch_context also works with ranged based for-loops but using the
		 * pg::lex::gmatch function is prefered.
		 *
		 * \note A lex context keeps a reference to the input string and pattern.
		 *
		 * \tparam StrCharT The char type of the input string.
		 * \tparam PatCharT The char type of the pattern.
		 *
		 * \see pg::lex::gmatch
		 * \see pg::lex::gmatch_iterator
		 * \see pg::lex::begin
		 * \see pg::lex::end
		 */
		template <typename StrCharT, typename PatCharT>
		struct gmatch_context
		{
			const detail::string_context<StrCharT> s;
			const pattern<PatCharT> p;

			template <typename StrT, typename PatCharT_>
			gmatch_context(StrT && s, const pattern<PatCharT_> & p) noexcept
				: s(std::forward<StrT>(s))
				, p(p)
			{}

			PG_LEX_NODISCARD bool operator ==(const gmatch_context<StrCharT, PatCharT>& other) const noexcept
			{
				return s.begin == other.s.begin && s.end == other.s.end &&
					   p.begin == other.p.begin && p.end == other.p.end;
			}
		};

#if defined(__cpp_deduction_guides)
		template <typename StrT, typename PatCharT>
		gmatch_context(StrT &&, const pattern<PatCharT> &) noexcept ->
		gmatch_context<detail::char_type<StrT>, PatCharT>;
#endif

		/**
		 * \brief An iterator for matches in pg::lex::context objects.
		 *
		 * The constructor does not perform the first match so the match result is not yet valid.
		 * First you must increase the iterator before dereferencing it to search for the first match result.
		 *
		 * The iterator behaves as a forward iterator and can only advance with the `++` operator.
		 *
		 * \see pg::lex::gmatch
		 * \see pg::lex::gmatch_context
		 * \see pg::lex::begin
		 * \see pg::lex::end
		 */
		template <typename StrCharT, typename PatCharT>
		struct gmatch_iterator
		{
			gmatch_iterator(const gmatch_context<StrCharT, PatCharT> & ctx, const StrCharT * start) noexcept
				: c(ctx)
				, pos(start)
			{}

			/**
			 * \brief Iterates to the next match in the context.
			 *
			 * The match result is empty when the end is reached.
			 */
			gmatch_iterator& operator ++()
			{
				detail::match_state<StrCharT, PatCharT> ms(c.s.begin, c.s.end, c.p.end, mr);

				while(pos <= c.s.end)
				{
					const StrCharT * e;
					bool r;
					std::tie(e, r) = detail::match(ms, pos, c.p.begin);
					if(!r || e == last_match)
					{
						pos = e + 1;
						ms.reprepstate();
					}
					else
					{
						ms.pos = { static_cast<int>(pos - c.s.begin), static_cast<int>(e - c.s.begin) };
						if(ms.level == 0)
						{
							ms.captures[ms.level].init(pos);
							ms.captures[ms.level].len(static_cast<int>(e - pos));
							++ms.level;
						}
						last_match = e;
						pos = e;

						return *this;
					}
				}

				return *this;
			}

			PG_LEX_NODISCARD bool operator ==(const gmatch_iterator & other) const noexcept
			{
				return c == other.c && pos == other.pos;
			}

			PG_LEX_NODISCARD bool operator !=(const gmatch_iterator & other) const noexcept
			{
				return !(*this == other);
			}

			/**
			 * \brief Dereferences to a match result.
			 */
			PG_LEX_NODISCARD const auto & operator *() const noexcept
			{
				return mr;
			}

			/**
			 * \brief Returns a pointer to a match result.
			 */
			PG_LEX_NODISCARD auto operator ->() const noexcept
			{
				return &mr;
			}

		private:
			const gmatch_context<StrCharT, PatCharT> c;
			const StrCharT * pos = nullptr;
			const StrCharT * last_match = nullptr;
			basic_match_result<StrCharT> mr;
		};

		/**
		 * \brief Returns a pg::lex::gmatch_iterator from a lex context object.
		 *
		 * When the context contains matches, the iterator has initial the result of the first match.
		 * The iterator is equal to the end iterator when the context has no matches.
		 *
		 * \see pg::lex::gmatch_iterator
		 * \see pg::lex::end
		 */
		template <typename StrCharT, typename PatCharT>
		PG_LEX_NODISCARD auto begin(const gmatch_context<StrCharT, PatCharT> & c)
		{
			auto it = gmatch_iterator<StrCharT, PatCharT>(c, c.s.begin);
			return ++it;
		}

		/**
		 * \brief Returns a pg::lex::gmatch_iterator that indicates the end of a lex context.
		 *
		 * The result of the end iterator is always an empty match result.
		 *
		 * \see pg::lex::gmatch_iterator
		 * \see pg::lex::begin
		 */
		template <typename StrCharT, typename PatCharT>
		PG_LEX_NODISCARD auto end(const gmatch_context<StrCharT, PatCharT> & c) noexcept
		{
			return gmatch_iterator<StrCharT, PatCharT>(c, c.s.end + 1);
		}

		/**
		 * \brief Returns a pg::lex::gmatch_context that is used to iterate with a pattern over string.
		 *
		 * This function is most likely to be used with a range-based for.
		 *
		 * \see pg::lex::gmatch_context
		 */
		template <typename StrT, typename PatCharT>
		PG_LEX_NODISCARD auto gmatch_pat(StrT && str, const pattern<PatCharT> & pat) noexcept
		{
			return gmatch_context<detail::char_type<StrT>, PatCharT>(std::forward<StrT>(str), pat);
		}

		/**
		 * \brief Returns a pg::lex::gmatch_context that is used to iterate with a pattern over string.
		 *
		 * This function is most likely to be used with a range-based for.
		 *
		 * \see pg::lex::gmatch_context
		 */
		template <typename StrT, typename PatT>
		PG_LEX_NODISCARD auto gmatch(StrT && str, PatT && pat) noexcept
		{
			using PatCharT = detail::char_type<PatT>;

			return gmatch_pat(std::forward<StrT>(str), pattern<PatCharT>(pat));
		}

		/**
		 * \brief Searches for the first match of a pattern in an input string.
		 *
		 * \return Returns a match result based on the character type of the input string.
		 */
		template <typename StrT, typename PatCharT>
		PG_LEX_NODISCARD auto match_pat(StrT && str, const pattern<PatCharT> & pat)
		{
			using str_char_type = detail::char_type<StrT>;

			basic_match_result<str_char_type> mr;
			const auto c = gmatch_pat(std::forward<StrT>(str), pat);
			detail::match_state<str_char_type, PatCharT> ms = { c.s.begin, c.s.end, c.p.end, mr };
			const str_char_type * pos = c.s.begin;

			do
			{
				const str_char_type * e;
				bool r;
				std::tie(e, r) = detail::match(ms, pos, c.p.begin);
				if(r)
				{
					ms.pos = { static_cast<int>(pos - c.s.begin), static_cast<int>(e - c.s.begin) };
					if(ms.level == 0)
					{
						ms.captures[ms.level].init(pos);
						ms.captures[ms.level].len(static_cast<int>(e - pos));
						++ms.level;
					}

					return mr;
				}
				else
				{
					pos = e + 1;
				}
			}
			while(pos <= c.s.end && !c.p.anchor);

			return mr;
		}

		template <typename StrT, typename PatT>
		PG_LEX_NODISCARD auto match(StrT && str, PatT && pat)
		{
			using PatCharT = detail::char_type<PatT>;

			return match_pat(std::forward<StrT>(str), pattern<PatCharT>(pat));
		}

		/**
		 * \brief Substitutes a replacement pattern for a match found in the input string.
		 *
		 * \param str The input string.
		 * \param pat The pattern used to find matches in the input string.
		 * \param repl The replacement pattern that substitutes the match.
		 * \param count The maximum number of substitutes; negative for unlimited an unlimited count.
		 *
		 * \return Returns a std::string based on the character type of the input string.
		 */
		template <typename StrT, typename PatCharT, typename ReplT>
			PG_LEX_NODISCARD auto gsub_pat(StrT && str, const pattern<PatCharT> & pat, ReplT && repl, int count = -1)
		{
			using str_char_type = detail::char_type<StrT>;
			using repl_char_type = detail::char_type<ReplT>;
			using iterator_type = gmatch_iterator<str_char_type, PatCharT>;

			const detail::string_context<repl_char_type> r(std::forward<ReplT>(repl));
			const auto c = gmatch_pat(std::forward<StrT>(str), pat);
			iterator_type match_it(c, c.s.begin);
			const auto match_end_it = end(c);

			std::basic_string<str_char_type> result;
			result.reserve(c.s.end - c.s.begin);

			const str_char_type * copy_begin = c.s.begin;
			while(++match_it != match_end_it && count != 0)
			{
				--count;

				const basic_match_result<str_char_type> & mr = *match_it;
				const str_char_type * copy_end = c.s.begin + mr.position().first;

				result.append(copy_begin, copy_end);

				const repl_char_type *r_begin = r.begin;
				for(const repl_char_type *find = std::find(r_begin, r.end, '%') ;
					find != r.end ;
					r_begin = find + 1, find = std::find(r_begin, r.end, '%'))
				{
					result.append(r_begin, find); // Copy pattern before '%'
					++find; // skip ESC

					const str_char_type cap_char = *find;
					if(cap_char == '%') // %%
					{
						result.append(1u, cap_char);
					}
					else if(cap_char == '0') // %0
					{
						const str_char_type * const match_begin = c.s.begin + mr.position().first;
						const str_char_type * const match_end = c.s.begin + mr.position().second;
						result.append(match_begin ,match_end);
					}
					else if(std::isdigit(cap_char)) // %n
					{
						const auto cap_index = static_cast<size_t>(cap_char - '1');
						if(cap_index >= mr.size()) PG_LEX_UNLIKELY
						{
							throw lex_error(capture_invalid_index);
						}
						const auto cap = mr.at(cap_index);
						if(cap.size() == 0) // Position capture
						{
							const ptrdiff_t pos = 1 + cap.data() - c.s.begin;
							detail::append_number(result, pos);
						}
						else
						{
							result.append(cap.data(), cap.size());
						}
					}
					else
					{
						throw lex_error(percent_invalid_use_in_replacement);
					}
				}

				result.append(r_begin, r.end);

				copy_begin = c.s.begin + mr.position().second;
			}

			result.append(copy_begin, c.s.end);

			return result;
		}

		template <typename StrT, typename PatT, typename ReplT>
		PG_LEX_NODISCARD auto gsub(StrT&& str, PatT && pat, ReplT && repl, int count = -1)
		{
			using PatCharT = detail::char_type<PatT>;

			return gsub_pat(std::forward<StrT>(str), pattern<PatCharT>(pat), std::forward<ReplT>(repl), count);
		}

		/**
		 * \brief Substitutes a replacement for a match found in the input string.
		 *
		 * \param str The input string.
		 * \param pat The pattern used to find matches in the input string.
		 * \param repl A function that accepts a match result and returns the replacement.
		 * \param count The maximum number of substitutes; negative for unlimited an unlimited count.
		 *
		 * \return Returns a std::string based on the character type of the input string.
		 */
		template <typename StrT, typename PatCharT, typename Function>
		PG_LEX_NODISCARD auto gsub_pat_func(StrT && str, const pattern<PatCharT> & pat, Function && func, int count = -1)
		{
			using str_char_type = detail::char_type<StrT>;
			using iterator_type = gmatch_iterator<str_char_type, PatCharT>;

			const auto c = gmatch_pat(std::forward<StrT>(str), pat);
			auto match_it = iterator_type(c, c.s.begin);
			const auto match_end_it = end(c);

			std::basic_string<str_char_type> result;
			result.reserve(c.s.end - c.s.begin);

			const str_char_type * copy_begin = c.s.begin;
			while(++match_it != match_end_it && count != 0)
			{
				--count;

				const auto & mr = *match_it;
				const str_char_type * copy_end = c.s.begin + mr.position().first;

				result.append(copy_begin, copy_end);
				result.append(func(mr));

				copy_begin = c.s.begin + mr.position().second;
			}

			result.append(copy_begin, c.s.end);

			return result;
		}

		template <typename StrT, typename PatT, typename Function>
		PG_LEX_NODISCARD auto gsub_func(StrT && str, PatT && pat, Function && func, int count = -1)
		{
			using PatCharT = detail::char_type<PatT>;

			return gsub_pat_func(std::forward<StrT>(str), pattern<PatCharT>(pat), std::forward<Function>(func), count);
		}
	}
}

/**
 * drec_packrat.hpp
 *
 * Copyright (c) 2018-2019 Peter Lenkefi
 * Distributed under the MIT License.
 *
 * A packrat parser that is capable of direct left-recursion.
 */

#ifndef CPPCMB_PARSERS_DREC_PACKRAT_HPP
#define CPPCMB_PARSERS_DREC_PACKRAT_HPP

#include "packrat.hpp"

namespace cppcmb {

namespace detail {

// XXX(LPeter1997): This is a more generic structure, shouldn't be here!
template <typename T, typename Tag>
class tagged_wrapper {
private:
    cppcmb_self_check(tagged_wrapper);

    T m_Value;

public:
    template <typename TFwd>
    constexpr tagged_wrapper(TFwd&& val)
        noexcept(std::is_nothrow_constructible_v<T, TFwd&&>)
        : m_Value(cppcmb_fwd(val)) {
    }

    cppcmb_getter(value, m_Value)
};

} /* namespace detail */

template <typename P>
class drec_packrat_t : public detail::packrat_base<drec_packrat_t<P>> {
private:
    cppcmb_self_check(drec_packrat_t);

    template <typename T>
    using base_recursion = detail::tagged_wrapper<T, struct drec_base_tag>;

    template <typename T>
    using in_recursion = detail::tagged_wrapper<T, struct drec_in_tag>;

    P m_Parser;

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    [[nodiscard]]
    constexpr decltype(auto) grow(
        reader<Src> const& r,
        parser_result_t<P, Src>& old_res
    ) const {

        // XXX(LPeter1997): Right now the max_furthest is explicitly written
        // back into the cache (redundantly). We could always ask the result
        // directly.
        // That way we don't have to put_memo the old values just to update
        // 'furthest'

        using in_rec = in_recursion<parser_result_t<P, Src>>;

        if (old_res.is_failure()) {
            return old_res;
        }
        auto& old_succ = old_res.success();

        auto tmp_res = m_Parser.apply(r);
        auto max_furthest = std::max(old_res.furthest(), tmp_res.furthest());

        // Update the furthest value in both entries
        old_res.m_Furthest = max_furthest;
        tmp_res.m_Furthest = max_furthest;

        if (tmp_res.is_success()) {
            auto& tmp_succ = tmp_res.success();
            if (old_succ.matched() < tmp_succ.matched()) {
                // We successfully grew the seed
                auto& new_old = this->put_memo(
                    r, in_rec(tmp_res), max_furthest
                ).value();
                return grow(r, new_old);
            }
            // We need to overwrite max-furthest in the memo-table!
            // That's why we don't simply return old_res
            return this->put_memo(
                r, in_rec(old_res), max_furthest
            ).value();
        }
        // We need to overwrite max-furthest in the memo-table!
        // That's why we don't simply return old_res
        return this->put_memo(
            r, in_rec(old_res), max_furthest
        ).value();
    }

public:
    template <typename PFwd, cppcmb_requires_t(!is_self_v<PFwd>)>
    constexpr drec_packrat_t(PFwd&& p)
        noexcept(std::is_nothrow_constructible_v<P, PFwd&&>)
        : m_Parser(cppcmb_fwd(p)) {
    }

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    [[nodiscard]] constexpr decltype(auto) apply(reader<Src> const& r) const {
        cppcmb_assert_parser(P, Src);

        using result_t = parser_result_t<P, Src>;
        using base_rec = std::pair<base_recursion<result_t>, bool>;
        using in_rec = in_recursion<result_t>;

        auto* entry = this->get_memo(r);
        if (entry == nullptr) {
            // Nothing is in the cache yet, write a dummy error
            this->put_memo(r, base_rec(result_t(failure(), 0U), true), 0U);
            // Now invoke the parser
            // If it's recursive, the entry must have changed
            auto tmp_res = m_Parser.apply(r);
            // Refresh entry
            entry = this->get_memo(r);
            cppcmb_assert(
                "The entry cannot be nullptr here!",
                entry != nullptr
            );

            // Check for change
            if (auto* inr = std::any_cast<in_rec>(entry)) {
                // We are in recursion
                auto& res = this->put_memo(
                    r, in_rec(std::move(tmp_res)), tmp_res.furthest()
                ).value();
                return grow(r, res);
            }
            // Base-thing, no progress
            // Overwrite the base-type to contain the result
            auto* br = std::any_cast<base_rec>(entry);
            cppcmb_assert(
                "A direct-packrat parser must either enter a "
                "'base' or 'in'-recursion entry!",
                br != nullptr
            );
            return this->put_memo(
                r,
                base_rec(std::move(tmp_res), false),
                tmp_res.furthest()
            ).first.value();
        }
        else {
            // Something is in the cache
            if (auto* br_p = std::any_cast<base_rec>(entry)) {
                auto& br = *br_p;
                if (br.second) {
                    // Recursion signal
                    return this->put_memo(
                        r,
                        in_rec(result_t(failure(), 0U)),
                        0U
                    ).value();
                }
                return br.first.value();
            }
            else {
                auto* inr = std::any_cast<in_rec>(entry);
                cppcmb_assert(
                    "A direct-packrat parser must either enter a "
                    "'base' or 'in'-recursion entry!",
                    inr != nullptr
                );
                return inr->value();
            }
        }
    }
};

template <typename PFwd>
drec_packrat_t(PFwd) -> drec_packrat_t<PFwd>;

/**
 * Wrapper to make any combinator a direct-left-recursive packrat parser.
 */
template <typename PFwd>
[[nodiscard]] constexpr auto memo_d(PFwd&& p)
    cppcmb_return(drec_packrat_t(cppcmb_fwd(p)))

struct as_memo_d_t {};

inline constexpr auto as_memo_d = as_memo_d_t();

// Direct-left-recursive packrat
template <typename P, cppcmb_requires_t(detail::is_combinator_cvref_v<P>)>
constexpr auto operator%=(P&& parser, as_memo_d_t)
    cppcmb_return(memo_d(cppcmb_fwd(parser)))

} /* namespace cppcmb */

#endif /* CPPCMB_PARSERS_DREC_PACKRAT_HPP */

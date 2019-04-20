/**
 * irec_packrat.hpp
 *
 * Copyright (c) 2018-2019 Peter Lenkefi
 * Distributed under the MIT License.
 *
 * A packrat parser that is capable of indirect left-recursion.
 */

#ifndef CPPCMB_PARSERS_IREC_PACKRAT_HPP
#define CPPCMB_PARSERS_IREC_PACKRAT_HPP

#include "packrat.hpp"

namespace cppcmb {

template <typename P>
class irec_packrat_t : public detail::packrat_base<irec_packrat_t<P>> {
private:
    cppcmb_self_check(irec_packrat_t);

    using head = detail::irec_head;
    using left_recursive = detail::irec_left_recursive;

    P m_Parser;

    // XXX(LPeter1997): decltype(auto) where possible

    // XXX(LPeter1997): Noexcept specifier
    // XXX(LPeter1997): This could be static
    template <typename T>
    constexpr decltype(auto) to_result(std::any& a) const {
        if (auto* r = std::any_cast<std::shared_ptr<left_recursive>>(&a)) {
            return std::any_cast<T&>((*r)->seed());
        }
        else {
            return std::any_cast<T&>(a);
        }
    }

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    /* constexpr */ auto recall(reader<Src> const& r) const
        -> std::optional<std::any> {
        using return_t = parser_result_t<P, Src>;

        auto& heads = r.context().call_heads();

        auto* cached = this->get_memo(r);
        auto* in_heads = heads.get(r);

        if (in_heads == nullptr) {
            if (cached == nullptr) {
                return std::nullopt;
            }
            else {
                return *cached;
            }
        }
        else {
            auto& h = *in_heads;

            if (cached == nullptr && !(
                    this->original_id() == h.head_id()
                || detail::contains(h.involved_set(), this->original_id())
            )) {
                return return_t(failure(), 0U);
            }

            auto it = h.eval_set().cend();
            if (detail::contains(h.eval_set(), this->original_id(), it)) {
                // Remove the rule id from the evaluation id set of the head
                h.eval_set().erase(it);
                auto tmp_res = m_Parser.apply(r);
                *cached = tmp_res;
            }

            return *cached;
        }
    }

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    constexpr void setup_lr(
        reader<Src> const& r,
        left_recursive& rec_detect) const {

        if (!rec_detect.head()) {
            rec_detect.head() = head(this->original_id());
        }
        auto& lr_stack = r.context().call_stack();
        for (
            auto it = lr_stack.begin();
            it != lr_stack.end() && (*it)->parser_id() != this->original_id();
            ++it) {

            (*it)->head() = rec_detect.head();
            rec_detect.head()->involved_set().insert((*it)->parser_id());
        }
    }

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    constexpr auto lr_answer(
        reader<Src> const& r,
        left_recursive& growable) const {

        using return_t = parser_result_t<P, Src>;
        cppcmb_assert(
            "The growable must contain a head!",
            growable.head().has_value()
        );

        auto& h = *growable.head();
        auto& seed = to_result<return_t>(growable.seed());

        if (h.head_id() != this->original_id()) {
            return seed;
        }
        else {
            auto& s = this->put_memo(r, seed, seed.furthest());
            if (s.is_failure()) {
                return s;
            }
            else {
                return grow(r, s, h);
            }
        }
    }

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    constexpr auto grow(
        reader<Src> const& r,
        parser_result_t<P, Src>& old_res,
        head& h) const -> parser_result_t<P, Src> {

        // XXX(LPeter1997): Right now the max_furthest is explicitly written
        // back into the cache (redundantly). We could always ask the result
        // directly.
        // That way we don't have to put_memo the old values just to update
        // 'furthest'
        // Same comment and TODO as in drec_packrat_t::grow

        auto& rec_heads = r.context().call_heads();

        rec_heads[r] = &h;
        h.eval_set() = h.involved_set();

        std::size_t old_cur = 0U;
        if (old_res.is_success()) {
            old_cur = old_res.success().matched();
        }

        auto tmp_res = m_Parser.apply(r);
        auto max_furthest = std::max(old_res.furthest(), tmp_res.furthest());

        // Update the furthest value in both entries
        old_res.m_Furthest = max_furthest;
        tmp_res.m_Furthest = max_furthest;

        if (tmp_res.is_success()) {
            auto& tmp_succ = tmp_res.success();
            if (old_cur < tmp_succ.matched()) {
                auto& new_old = this->put_memo(
                    r, tmp_res, max_furthest
                );
                return grow(r, new_old, h);
            }
            else {
                // We need to overwrite max-furthest in the memo-table!
                // That's why we don't simply return old_res

                auto it = rec_heads.find(r);
                cppcmb_assert("", it != rec_heads.end());
                rec_heads.erase(it);

                // auto* val = this->get_memo(r);
                // cppcmb_assert("", val != nullptr);

                // return this-> template to_result<return_t>(*val);
                return this->put_memo(r, std::move(old_res), max_furthest);
            }
        }
        else {
            // We need to overwrite max-furthest in the memo-table!
            // That's why we don't simply return old_res

            auto it = rec_heads.find(r);
            cppcmb_assert("", it != rec_heads.end());
            rec_heads.erase(it);

            return this->put_memo(r, std::move(old_res), max_furthest);
        }
    }

public:
    template <typename PFwd, cppcmb_requires_t(!is_self_v<PFwd>)>
    constexpr irec_packrat_t(PFwd&& p)
        noexcept(std::is_nothrow_constructible_v<P, PFwd&&>)
        : m_Parser(cppcmb_fwd(p)) {
    }

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    [[nodiscard]]
    constexpr auto apply(reader<Src> const& r) const {
        cppcmb_assert_parser(P, Src);

        using return_t = parser_result_t<P, Src>;

        auto& lr_stack = r.context().call_stack();

        auto m = recall(r);
        if (!m) {
            auto base = std::make_shared<left_recursive>(
                return_t(failure(), 0U), this->original_id()
            );
            lr_stack.push_front(base);
            this->put_memo(r, base, 0U);
            auto tmp_res = m_Parser.apply(r);
            lr_stack.pop_front();

            if (!base->head()) {
                return this->put_memo(r, tmp_res, tmp_res.furthest());
            }
            else {
                base->seed() = tmp_res;
                return lr_answer(r, *base);
            }
        }
        else {
            auto& entry = *m;
            if (auto* lr =
                std::any_cast<std::shared_ptr<left_recursive>>(&entry)) {

                setup_lr(r, **lr);
                return this-> template to_result<return_t>((*lr)->seed());
            }
            else {
                return this-> template to_result<return_t>(entry);
            }
        }
    }
};

template <typename PFwd>
irec_packrat_t(PFwd) -> irec_packrat_t<PFwd>;

/**
 * Wrapper to make any combinator an indirect-left-recursive packrat parser.
 */
template <typename PFwd>
[[nodiscard]] constexpr auto memo_i(PFwd&& p)
    cppcmb_return(irec_packrat_t(cppcmb_fwd(p)))

struct as_memo_i_t {};

inline constexpr auto as_memo_i = as_memo_i_t();

// Indirect-left-recursive packrat
template <typename P, cppcmb_requires_t(detail::is_combinator_cvref_v<P>)>
constexpr auto operator%=(P&& parser, as_memo_i_t)
    cppcmb_return(memo_i(cppcmb_fwd(parser)))

} /* namespace cppcmb */

#endif /* CPPCMB_PARSERS_IREC_PACKRAT_HPP */

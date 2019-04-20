/**
 * memo_context.hpp
 *
 * Copyright (c) 2018-2019 Peter Lenkefi
 * Distributed under the MIT License.
 *
 * All the helper structures for memorization/incremental parsing.
 */

#ifndef CPPCMB_MEMO_CONTEXT_HPP
#define CPPCMB_MEMO_CONTEXT_HPP

#include <any>
#include <cstddef>
#include <deque>
#include <optional>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>
#include "detail.hpp"
#include "reader.hpp"

// XXX(LPeter1997): Move operations from the parsers to here
// The structures should be aware of their usage, they shouldn't be just
// wrappers around STL containers...

namespace cppcmb {

namespace detail {

// XXX(LPeter1997): Noexcept specifier
/**
 * Functionality for hashing a pair. Straight from Boost.
 */
template <typename T>
constexpr void hash_combine(std::size_t& seed, T const& v) {
    seed ^= std::hash<T>()(v) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
}

struct pair_hasher {
    // XXX(LPeter1997): Noexcept specifier
    template <typename T1, typename T2>
    constexpr auto operator()(std::pair<T1, T2> const& p) const {
        std::size_t seed = 0;
        hash_combine(seed, p.first);
        hash_combine(seed, p.second);
        return seed;
    }
};

/**
 * Memorization table for packrat parsers.
 */
class memo_table {
private:
    // pair<parser identifier, position>
    using key_type = std::pair<std::uintptr_t, std::size_t>;
    // pair<result, furthest>
    using value_type = std::pair<std::any, std::size_t>;

    std::unordered_map<key_type, value_type, pair_hasher> m_Cache;

public:
    // XXX(LPeter1997): Noexcept specifier
    [[nodiscard]]
    /* constexpr */ std::any* get(std::uintptr_t pid, std::size_t pos) {
        auto it = m_Cache.find({ pid, pos });
        if (it == m_Cache.end()) {
            return nullptr;
        }
        return &it->second.first;
    }

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    [[nodiscard]]
    constexpr std::any* get(std::uintptr_t pid, reader<Src> const& r) {
        return get(pid, r.cursor());
    }

    // XXX(LPeter1997): Noexcept specifier
    template <typename TFwd>
    constexpr auto& put(std::uintptr_t pid, std::size_t pos,
        TFwd&& val, std::size_t furth) {

        using raw_type = remove_cvref_t<TFwd>;
        auto id = std::pair(pid, pos);
        auto& a = (m_Cache[id] = std::pair(cppcmb_fwd(val), furth));
        return std::any_cast<raw_type&>(a.first);
    }

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src, typename TFwd>
    constexpr auto& put(std::uintptr_t pid, reader<Src> const& r,
        TFwd&& val, std::size_t furth) {

        return put(pid, r.cursor(), cppcmb_fwd(val), furth);
    }

    // XXX(LPeter1997): Noexcept specifier
    /* constexpr */ void clear() {
        m_Cache.clear();
    }

    // XXX(LPeter1997): Noexcept specifier
    void invalidate(std::size_t start, std::size_t rem, std::size_t ins) {
        // start: Position of the source we are manipulating
        // rem: Removed length
        // ins: Inserted length

        auto end = start + rem;

        // XXX(LPeter1997): Going through every entry is not very effective
        // we would need some helper structure to search by interval

        for (auto it = m_Cache.begin(); it != m_Cache.end();) {
            auto r_from = it->first.second;
            // XXX(LPeter1997): Solve this
            // Maybe redundantly store it
            auto r_furthest = it->second.second;
            auto r_to = r_from + r_furthest;

            // [f_from; r_to) is the entry's interval
            // Need to check overlap with [start; end)
            // If they overlap, remove entry

            // XXX(LPeter1997): Allow equality?
            if (start > r_to || r_from > end) {
                // No overlap
                ++it;
            }
            else {
                // Overlapping
                it = m_Cache.erase(it);
            }
        }

        // XXX(LPeter1997): THIS IS HORRIBLE FOR PERFORMANCE
        // WE ARE REMOVING THEN PUTTING BACK EVERY ENTRY THAT IS AFTER THE
        // EDIT
        // XXX(LPeter1997): This is a very ineffective implementation right
        // now. It's just to test the algorithm itself
        std::intptr_t diff = std::intptr_t(ins) - std::intptr_t(rem);
        // Collect and erase entries that need to be shifted
        std::vector<std::pair<key_type, value_type>> to_shift;
        for (auto it = m_Cache.begin(); it != m_Cache.end();) {
            auto r_from = it->first.second;
            if (r_from >= start) {
                to_shift.push_back({
                    { it->first.first, it->first.second },
                    std::move(it->second)
                });
                it = m_Cache.erase(it);
            }
            else {
                ++it;
            }
        }
        // Re-insert the entries
        for (auto& [k, v] : to_shift) {
            auto& [p_id, pos] = k;
            m_Cache.insert({ { p_id, pos + diff }, std::move(v) });
        }
        // END OF UNGODLY INEFFICIENT CODE
    }
};

class irec_head {
private:
    std::uintptr_t                     m_HeadID;
    std::unordered_set<std::uintptr_t> m_InvolvedIDSet;
    std::unordered_set<std::uintptr_t> m_EvalIDSet;

public:
    // XXX(LPeter1997): Noexcept specifier
    explicit /* constexpr */ irec_head(std::uintptr_t hid)
        : m_HeadID(hid) {
    }

    [[nodiscard]] constexpr std::uintptr_t head_id() const noexcept {
        return m_HeadID;
    }

    cppcmb_getter(involved_set, m_InvolvedIDSet)
    cppcmb_getter(eval_set, m_EvalIDSet)
};

class irec_left_recursive {
private:
    std::any                 m_Seed;
    std::uintptr_t           m_ParserID;
    std::optional<irec_head> m_Head;

public:
    // XXX(LPeter1997): Noexcept specifier
    template <typename TFwd>
    constexpr irec_left_recursive(TFwd&& seed, std::uintptr_t pid)
        : m_Seed(cppcmb_fwd(seed)), m_ParserID(pid) {
    }

    cppcmb_getter(seed, m_Seed)
    cppcmb_getter(head, m_Head)

    [[nodiscard]] constexpr std::uintptr_t parser_id() const noexcept {
        return m_ParserID;
    }
};

/**
 * A type to track call-heads.
 */
class call_head_table {
private:
    std::unordered_map<std::size_t, irec_head*> m_Heads;

public:
    // XXX(LPeter1997): Noexcept specifier
    [[nodiscard]]
    /* constexpr */ irec_head* get(std::size_t n) const {
        auto it = m_Heads.find(n);
        if (it == m_Heads.end()) {
            return nullptr;
        }
        else {
            return it->second;
        }
    }

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    [[nodiscard]]
    /* constexpr */ irec_head* get(reader<Src> const& r) const {
        return get(r.cursor());
    }

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    constexpr decltype(auto) operator[](reader<Src> const& r) {
        return m_Heads[r.cursor()];
    }

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    [[nodiscard]] constexpr auto find(reader<Src> const& r) {
        return m_Heads.find(r.cursor());
    }

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    [[nodiscard]] constexpr auto find(reader<Src> const& r) const {
        return m_Heads.find(r.cursor());
    }

    // XXX(LPeter1997): Noexcept specifier
    [[nodiscard]] /* constexpr */ auto begin() { return m_Heads.begin(); }
    // XXX(LPeter1997): Noexcept specifier
    [[nodiscard]] /* constexpr */ auto begin() const { return m_Heads.begin(); }
    // XXX(LPeter1997): Noexcept specifier
    [[nodiscard]] /* constexpr */ auto end() { return m_Heads.end(); }
    // XXX(LPeter1997): Noexcept specifier
    [[nodiscard]] /* constexpr */ auto end() const { return m_Heads.end(); }

    // XXX(LPeter1997): Noexcept specifier
    template <typename It>
    constexpr void erase(It it) {
        m_Heads.erase(it);
    }

    // XXX(LPeter1997): Noexcept specifier
    void clear() {
        m_Heads.clear();
    }
};

class call_stack {
private:
    std::deque<std::shared_ptr<irec_left_recursive>> m_Stack;

public:
    // XXX(LPeter1997): Noexcept specifier
    template <typename TFwd>
    constexpr void push_front(TFwd&& val) {
        m_Stack.push_front(val);
    }

    // XXX(LPeter1997): Noexcept specifier
    /* constexpr */ void pop_front() {
        m_Stack.pop_front();
    }

    // XXX(LPeter1997): Noexcept specifier
    [[nodiscard]] /* constexpr */ auto begin() { return m_Stack.begin(); }
    // XXX(LPeter1997): Noexcept specifier
    [[nodiscard]] /* constexpr */ auto begin() const { return m_Stack.begin(); }
    // XXX(LPeter1997): Noexcept specifier
    [[nodiscard]] /* constexpr */ auto end() { return m_Stack.end(); }
    // XXX(LPeter1997): Noexcept specifier
    [[nodiscard]] /* constexpr */ auto end() const { return m_Stack.end(); }

    // XXX(LPeter1997): Noexcept specifier
    void clear() {
        m_Stack.clear();
    }
};

// XXX(LPeter1997): These are not really helper structures and don't belong to
// the memo context.
// Furthermore, they should belong to a structure instead.

template <typename Coll, typename Val, typename It>
constexpr bool contains(Coll const& coll, Val const& v, It& it) {
    it = coll.find(v);
    return it != coll.end();
}

template <typename Coll, typename Val>
constexpr bool contains(Coll const& coll, Val const& v) {
    auto it = coll.end();
    return contains(coll, v, it);
}

} /* namespace detail */

// XXX(LPeter1997): Probably don't need a full getter with all qualifiers
// Only need a mutable-lvalue getter for all 3 helpers
class memo_context {
private:
    detail::memo_table      m_MemoTable;
    detail::call_head_table m_RecursionHeads;
    detail::call_stack      m_LrStack;

public:
    cppcmb_getter(memo, m_MemoTable)
    cppcmb_getter(call_heads, m_RecursionHeads)
    cppcmb_getter(call_stack, m_LrStack)

    // XXX(LPeter1997): Noexcept specifier
    void clear() {
        m_MemoTable.clear();
        m_RecursionHeads.clear();
        m_LrStack.clear();
    }
};

} /* namespace cppcmb */

#endif /* CPPCMB_MEMO_CONTEXT_HPP */

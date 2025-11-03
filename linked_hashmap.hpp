/**
 * implement a container like std::linked_hashmap
 */
#ifndef SJTU_LINKEDHASHMAP_HPP
#define SJTU_LINKEDHASHMAP_HPP

// only for std::equal_to<T> and std::hash<T>
#include <functional>
#include <cstddef>
#include "utility.hpp"
#include "exceptions.hpp"

namespace sjtu {
    /**
     * In linked_hashmap, iteration ordering is differ from map,
     * which is the order in which keys were inserted into the map.
     * You should maintain a doubly-linked list running through all
     * of its entries to keep the correct iteration order.
     *
     * Note that insertion order is not affected if a key is re-inserted
     * into the map.
     */
    
template<
	class Key,
	class T,
	class Hash = std::hash<Key>, 
	class Equal = std::equal_to<Key>
> class linked_hashmap {
public:
	/**
	 * the internal type of data.
	 * it should have a default constructor, a copy constructor.
	 * You can use sjtu::linked_hashmap as value_type by typedef.
	 */
	typedef pair<const Key, T> value_type;
 
	/**
	 * see BidirectionalIterator at CppReference for help.
	 *
	 * if there is anything wrong throw invalid_iterator.
	 *     like it = linked_hashmap.begin(); --it;
	 *       or it = linked_hashmap.end(); ++end();
	 */
	class const_iterator;

	// internal node structure (list + hash links)
	struct node {
		value_type *data = nullptr; // nullptr for sentinel header
		node *prev = nullptr; // list prev
		node *next = nullptr; // list next
		node *hnext = nullptr; // hash chain next
		node() = default; // header
		node(const value_type &v) : data(new value_type(v)), prev(nullptr), next(nullptr), hnext(nullptr) {}
		~node() { if (data) delete data; }
	};
	class iterator {
	private:
		// pointer to the owning container and current node
		using map_type = linked_hashmap;
		map_type *owner = nullptr;
		node *cur = nullptr;
		friend class const_iterator;
		friend class linked_hashmap;
	public:
		// The following code is written for the C++ type_traits library.
		// Type traits is a C++ feature for describing certain properties of a type.
		// For instance, for an iterator, iterator::value_type is the type that the 
		// iterator points to. 
		// STL algorithms and containers may use these type_traits (e.g. the following 
		// typedef) to work properly. 
		// See these websites for more information:
		// https://en.cppreference.com/w/cpp/header/type_traits
		// About value_type: https://blog.csdn.net/u014299153/article/details/72419713
		// About iterator_category: https://en.cppreference.com/w/cpp/iterator
		using difference_type = std::ptrdiff_t;
		using value_type = typename linked_hashmap::value_type;
		using pointer = value_type*;
		using reference = value_type&;
		using iterator_category = std::output_iterator_tag;


		iterator() = default;
		iterator(map_type *o, node *c) : owner(o), cur(c) {}
		iterator(const iterator &other) = default;
		/**
		 * TODO iter++
		 */
		iterator operator++(int) {
			iterator tmp(*this);
			++(*this);
			return tmp;
		}
		/**
		 * TODO ++iter
		 */
		iterator & operator++() {
			if (owner == nullptr || cur == nullptr) throw invalid_iterator();
			if (cur == owner->header) throw invalid_iterator();
			cur = cur->next;
			return *this;
		}
		/**
		 * TODO iter--
		 */
		iterator operator--(int) {
			iterator tmp(*this);
			--(*this);
			return tmp;
		}
		/**
		 * TODO --iter
		 */
		iterator & operator--() {
			if (owner == nullptr || cur == nullptr) throw invalid_iterator();
			if (cur == owner->header->next) throw invalid_iterator();
			cur = (cur == owner->header) ? owner->header->prev : cur->prev;
			return *this;
		}
		/**
		 * a operator to check whether two iterators are same (pointing to the same memory).
		 */
		value_type & operator*() const {
		if (owner == nullptr || cur == nullptr || cur == owner->header) throw invalid_iterator();
		return *(cur->data);
		}
		bool operator==(const iterator &rhs) const { return owner == rhs.owner && cur == rhs.cur; }
		bool operator==(const const_iterator &rhs) const { return owner == rhs.owner && cur == rhs.cur; }
		/**
		 * some other operator for iterator.
		 */
		bool operator!=(const iterator &rhs) const { return !(*this == rhs); }
		bool operator!=(const const_iterator &rhs) const { return !(*this == rhs); }

		/**
		 * for the support of it->first. 
		 * See <http://kelvinh.github.io/blog/2013/11/20/overloading-of-member-access-operator-dash-greater-than-symbol-in-cpp/> for help.
		 */
		value_type* operator->() const noexcept { return cur->data; }
	};
 
	class const_iterator {
		// it should has similar member method as iterator.
		//  and it should be able to construct from an iterator.
		private:
			using map_type = linked_hashmap;
			const map_type *owner = nullptr;
			node *cur = nullptr;
			friend class iterator;
			friend class linked_hashmap;
		public:
			const_iterator() = default;
			const_iterator(const map_type *o, node *c) : owner(o), cur(c) {}
			const_iterator(const const_iterator &other) = default;
			const_iterator(const iterator &other) : owner(other.owner), cur(other.cur) {}
			const_iterator & operator=(const const_iterator &other) = default;
			const_iterator operator++(int) { const_iterator t(*this); ++(*this); return t; }
			const_iterator & operator++() {
				if (owner == nullptr || cur == nullptr) throw invalid_iterator();
				if (cur == owner->header) throw invalid_iterator();
				cur = cur->next; return *this;
			}
			const_iterator operator--(int) { const_iterator t(*this); --(*this); return t; }
			const_iterator & operator--() {
				if (owner == nullptr || cur == nullptr) throw invalid_iterator();
				if (cur == owner->header->next) throw invalid_iterator();
				cur = (cur == owner->header) ? owner->header->prev : cur->prev; return *this;
			}
		const value_type & operator*() const {
			if (owner == nullptr || cur == nullptr || cur == owner->header) throw invalid_iterator();
			return *(cur->data);
			}
			const value_type* operator->() const noexcept { return cur->data; }
			bool operator==(const const_iterator &rhs) const { return owner == rhs.owner && cur == rhs.cur; }
			bool operator!=(const const_iterator &rhs) const { return !(*this == rhs); }
			bool operator==(const iterator &rhs) const { return owner == rhs.owner && cur == rhs.cur; }
			bool operator!=(const iterator &rhs) const { return !(*this == rhs); }
			// And other methods in iterator.
			// And other methods in iterator.
			// And other methods in iterator.
	};
 
	/**
	 * TODO two constructors
	 */
	linked_hashmap() { init_empty(); }
	linked_hashmap(const linked_hashmap &other) { copy_from(other); }
 
	/**
	 * TODO assignment operator
	 */
	linked_hashmap & operator=(const linked_hashmap &other) {
		if (this == &other) return *this;
		clear();
		delete[] table;
		table = nullptr;
		if (header) { delete header; header = nullptr; }
		copy_from(other);
		return *this;
	}
 
	/**
	 * TODO Destructors
	 */
	~linked_hashmap() {
		clear();
		delete[] table;
		if (header) delete header;
	}
 
	/**
	 * TODO
	 * access specified element with bounds checking
	 * Returns a reference to the mapped value of the element with key equivalent to key.
	 * If no such element exists, an exception of type `index_out_of_bound'
	 */
	T & at(const Key &key) {
		node *p = find_node(key);
		if (!p) throw index_out_of_bound();
		return p->data->second;
	}
	const T & at(const Key &key) const {
		node *p = find_node(key);
		if (!p) throw index_out_of_bound();
		return p->data->second;
	}
 
	/**
	 * TODO
	 * access specified element 
	 * Returns a reference to the value that is mapped to a key equivalent to key,
	 *   performing an insertion if such key does not already exist.
	 */
	T & operator[](const Key &key) {
		node *p = find_node(key);
		if (p) return p->data->second;
		value_type v(key, T());
		return insert(v).first.operator->()->second;
	}
 
	/**
	 * behave like at() throw index_out_of_bound if such key does not exist.
	 */
	const T & operator[](const Key &key) const { return at(key); }
 
	/**
	 * return a iterator to the beginning
	 */
	iterator begin() { return iterator(this, header->next); }
	const_iterator cbegin() const { return const_iterator(this, header->next); }
 
	/**
	 * return a iterator to the end
	 * in fact, it returns past-the-end.
	 */
	iterator end() { return iterator(this, header); }
	const_iterator cend() const { return const_iterator(this, header); }
 
	/**
	 * checks whether the container is empty
	 * return true if empty, otherwise false.
	 */
	bool empty() const { return sz == 0; }
 
	/**
	 * returns the number of elements.
	 */
	size_t size() const { return sz; }
 
	/**
	 * clears the contents
	 */
	void clear() {
		// delete all nodes
		node *p = header ? header->next : nullptr;
		while (p && p != header) {
			node *nxt = p->next;
			delete p;
			p = nxt;
		}
		if (header) {
			header->next = header->prev = header;
		}
		// clear hash table chains
		if (table) {
			for (size_t i = 0; i < bucket_cnt; ++i) table[i] = nullptr;
		}
		sz = 0;
	}
 
	/**
	 * insert an element.
	 * return a pair, the first of the pair is
	 *   the iterator to the new element (or the element that prevented the insertion), 
	 *   the second one is true if insert successfully, or false.
	 */
	pair<iterator, bool> insert(const value_type &value) {
		node *exist = find_node(value.first);
		if (exist) return pair<iterator, bool>(iterator(this, exist), false);
		ensure_capacity(sz + 1);
		node *nd = new node(value);
		// link into list tail (before header)
		nd->prev = header->prev; nd->next = header;
		header->prev->next = nd; header->prev = nd;
		// link into hash bucket
		size_t idx = bucket_index(value.first);
		nd->hnext = table[idx]; table[idx] = nd;
		++sz;
		return pair<iterator, bool>(iterator(this, nd), true);
	}
 
	/**
	 * erase the element at pos.
	 *
	 * throw if pos pointed to a bad element (pos == this->end() || pos points an element out of this)
	 */
	void erase(iterator pos) {
		if (pos.owner != this || pos.cur == nullptr || pos.cur == header) throw invalid_iterator();
		node *p = pos.cur;
		// unlink from list
		p->prev->next = p->next;
		p->next->prev = p->prev;
		// unlink from hash bucket
		size_t idx = bucket_index(p->data->first);
		node *q = table[idx];
		node *pre = nullptr;
		while (q) {
			if (q == p) {
				if (pre) pre->hnext = q->hnext; else table[idx] = q->hnext;
				break;
			}
			pre = q; q = q->hnext;
		}
		delete p;
		--sz;
	}
 
	/**
	 * Returns the number of elements with key 
	 *   that compares equivalent to the specified argument,
	 *   which is either 1 or 0 
	 *     since this container does not allow duplicates.
	 */
	size_t count(const Key &key) const { return find_node(key) ? 1 : 0; }
 
	/**
	 * Finds an element with key equivalent to key.
	 * key value of the element to search for.
	 * Iterator to an element with key equivalent to key.
	 *   If no such element is found, past-the-end (see end()) iterator is returned.
	 */
	iterator find(const Key &key) { node *p = find_node(key); return p ? iterator(this, p) : end(); }
	const_iterator find(const Key &key) const { node *p = find_node(key); return p ? const_iterator(this, p) : cend(); }

private:

	size_t sz = 0;
	size_t bucket_cnt = 0;
	node **table = nullptr;
	node *header = nullptr; // sentinel node for doubly linked list
	Hash hasher = Hash();
	Equal equal = Equal();

	void init_empty() {
		bucket_cnt = 8;
		table = new node*[bucket_cnt];
		for (size_t i = 0; i < bucket_cnt; ++i) table[i] = nullptr;
		header = new node();
		header->next = header->prev = header;
		sz = 0;
	}

	void copy_from(const linked_hashmap &other) {
		bucket_cnt = other.bucket_cnt;
		table = new node*[bucket_cnt];
		for (size_t i = 0; i < bucket_cnt; ++i) table[i] = nullptr;
		header = new node();
		header->next = header->prev = header;
		sz = 0;
		for (node *p = other.header->next; p != other.header; p = p->next) {
			insert(*(p->data));
		}
	}

	size_t bucket_index(const Key &key) const { return hasher(key) % bucket_cnt; }

	node *find_node(const Key &key) const {
		if (bucket_cnt == 0) return nullptr;
		size_t idx = bucket_index(key);
		node *p = table[idx];
		while (p) {
			if (equal(p->data->first, key)) return p;
			p = p->hnext;
		}
		return nullptr;
	}

	void ensure_capacity(size_t need) {
		if (bucket_cnt == 0) init_empty();
		// rehash when load factor > 0.75
		if (need * 4 <= bucket_cnt * 3) return;
		size_t new_cnt = bucket_cnt * 2;
		node **newtab = new node*[new_cnt];
		for (size_t i = 0; i < new_cnt; ++i) newtab[i] = nullptr;
		// rechain all nodes according to new bucket size
		for (node *p = header->next; p != header; p = p->next) {
			size_t idx = hasher(p->data->first) % new_cnt;
			p->hnext = newtab[idx];
			newtab[idx] = p;
		}
		delete[] table;
		table = newtab;
		bucket_cnt = new_cnt;
	}

public:
    // no additional public members
};

}

#endif

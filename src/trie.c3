/**
 * @require Index.kindof == TypeKind.UNSIGNED_INT
 **/
module trie(<Value, Index>);
import std::collections::list;
import trie::bitmap;

def TrieNodeList = List(<TrieNode>);
def TriePath = List(<Index>) @private;

fault TrieError
{
    TRIE_FULL,
}

struct Trie
{
    TrieNodeList nodes;
}

struct TrieNode
{
    Index[256] children;
    Value value;
    bool valid;
}

fn void Trie.init(&self, usz initial_capacity = 8, Allocator* using = mem::heap())
{
    *self = {};
    self.nodes.init(initial_capacity, using);
    self.nodes.push(TrieNode{});
}

/**
 * @require self.nodes.len() > 0
 **/
fn Value! Trie.get(&self, char[] key)
{
    return self.nodes[0].get(self, key);
}

/**
 * @require self.nodes.len() > 0
 **/
fn void! Trie.set(&self, char[] key, Value value)
{
    self.nodes[0].set(self, key, value)!!;
}

/**
 * @require self.nodes.len() > 0
 **/
fn void! Trie.del(&self, char[] key)
{
    if (key.len == 0)
    {
        Value zero;
        (&self.nodes[0]).valid = false;
        return;
    }
    TriePath path;
    path.init(8, self.nodes.allocator);
    defer path.free();
    path.push(0);
    self.nodes[0].del(self, key, path)!;
}

fn Value! TrieNode.get(&self, Trie *t, char[] key) @private
{
    if (key.len == 0) return self.valid ? self.value : SearchResult.MISSING?;
    char c = key[0];
    Index idx = self.children[c];
    if (idx == 0) return SearchResult.MISSING?;
    return t.nodes[idx].get(t, key[1..]);
}

fn void! TrieNode.set(&self, Trie *t, char[] key, Value value) @private
{
    if (key.len == 0)
    {
        self.value = value;
        self.valid = true;
        return;
    }
    char c = key[0];
    Index idx = self.children[c];
    if (idx == 0)
    {
        usz new_idx = t.nodes.len();
        assert(new_idx != 0);
        if (new_idx > Index.max) return TrieError.TRIE_FULL?;
        idx = (Index)new_idx;
        self.children[c] = idx;
        t.nodes.push(TrieNode{});
    }
    t.nodes[idx].set(t, key[1..], value)!;
}

fn void! TrieNode.del(&self, Trie* t, char[] key, TriePath path) @private
{
    if (key.len > 0)
    {
        char c = key[0];
        Index idx = self.children[c];
        if (idx != 0)
        {
            path.push(idx);
            t.nodes[idx].del(t, key[1..], path)!;
        }
        return;
    }
    Index current = path.pop();
    for (isz i = path.len() - 1; i >= 0; i--)
    {
        Index parent = path[i];
        bool is_empty = true;
        foreach (j, p : t.nodes[parent].children)
        {
            if (p == current)
            {
                t.nodes[parent].children[j] = 0;
                (&t.nodes[p]).valid = false;
            }
            is_empty = is_empty && t.nodes[parent].children[j] == 0;
        }
        if (!is_empty) break;
    }
}

module trie::bitmap @private;

struct Bitmap
{
    ulong[4] data;
}

fn bool Bitmap.get(&self, char x)
{
    char i = x & 0b11;
    char j = 1 << (x >> 2);
    return self.data[i] & j != 0;
}

fn void Bitmap.set(&self, char x)
{
    char i = x & 0b11;
    char j = 1 << (x >> 2);
    self.data[i] |= j;
}

fn bool Bitmap.is_empty(&self)
{
    return self.data[0] == 0 && self.data[1] == 0 && self.data[2] == 0 && self.data[3] == 0;
}
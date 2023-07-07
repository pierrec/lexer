module trie_test @test;
import trie;

def Trie = Trie(<String, char>);

fn void test()
{
    Trie t;
    t.init();
    String! v = t.get("a");
    if (try v) assert(false, "key 'a' should not exist");
    if (catch t.set("a", "va")) assert(false, "key 'a' not added");
    String w = t.get("a")!!;
    assert(w == "va");

    if (catch t.set("hello world", "hi")) assert(false, "key not added");
    w = t.get("hello world")!!;
    assert(w == "hi");
    if (try invalid = t.get("hello")) assert(false, "key 'hello' should not exist");

    t.del("a")!!;
    String! x = t.get("a");
    if (try x) assert(false, "key 'a' should not exist");
}
## About

This is my implementation of Red-Black Trees for OCaml. It is based upon [_Red-Black Trees in a Functional Setting_](http://www.eecs.usma.edu/webs/people/okasaki/pubs.html#jfp99) by [Chris Okasaki](http://www.eecs.usma.edu/webs/people/okasaki/) in _Journal of Functional Programming_, 9(4):471-477, July 1999.

The Red-Black Trees are exposed via a map and a set API, which is designed to be compatible with the Map and Set modules in the OCaml standard library (which are implemented using AVL trees). You can use the [Rbmap](https://github.com/bmeurer/ocaml-rbtrees/blob/master/rbmap.ml) and [Rbset](https://github.com/bmeurer/ocaml-rbtrees/blob/master/rbset.ml) modules as drop-in replacement for the Map and Set modules.
 

## License

This implementation is licensed under the [Simplified BSD License](http://en.wikipedia.org/wiki/BSD_license). See the [LICENSE](http://github.com/bmeurer/ocaml-rbtress/raw/master/LICENSE) file for details.


## Bug Reports

If you come across any problems, please [create a ticket](http://github.com/bmeurer/ocaml-rbtrees/issues) and we will try to get it fixed as soon as possible.


## Contributing

Once you've made your commits:

1. [Fork](http://help.github.com/fork-a-repo/ "Fork a repo") ocaml-rbtrees.
2. Create a topic branch - `git checkout -b my_branch`.
3. Push to your topic branch - `git push origin my_branch`.
4. Create a [Pull Request](http://help.github.com/pull-requests/ "Send pull requests") from your topic branch.
5. That's it!


## Authors

Benedikt Meurer :: benedikt.meurer@googlemail.com :: [@bmeurer](http://twitter.com/bmeurer)


## Copyright

Copyright (c) 2007-2011 Benedikt Meurer. See the [License](http://github.com/bmeurer/ocaml-rbtrees/raw/master/LICENSE) file for details.


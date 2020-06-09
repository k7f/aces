//! [Algebra of Cause-Effect
//! Structures](https://link.springer.com/book/10.1007/978-3-030-20461-7)
//! &mdash; an implementation of the theory.  This is the core library
//! of the [_Ascesis_](https://docs.rs/ascesis) project.
//!
//! In an attempt to clarify some of the obscure parts of the
//! implementation of _aces_ library, several definitions are
//! collected below.
//!
//! These notes are, in a way, a draft of a &ldquo;low level&rdquo;
//! reformulation of the theory, not as concise and elegant as the
//! original but, hopefully, better suited for the design and analysis
//! of implementation details.  By describing elementary c-e
//! structures in a slightly more general context &mdash;
//! characterising them as a special case of _flowsets_ &mdash; one
//! may expect to better understand, for instance, the algorithms of
//! structural decomposition and search.
//!
//! # Flowsets
//!
//! Consider a _domain_ ***X*** &mdash; a non-empty and countable set
//! of _nodes_.  A _fork_ over ***X*** is any pair _(x, w)_, and a
//! _join_ is any pair _(w, x)_, where _x_ is an element of ***X***,
//! called an _eye_ of a fork (or join), and _w_ is a non-empty subset
//! of ***X***, called an _armset_ of a fork (or join).  We call
//! armset members _arms_ and say that any arm of a fork (join) is
//! _arming_ both the fork (join) and its eye.  A fork (join) is a
//! _loop_ iff its eye is in its armset.
//!
//! A _flowset_ over ***X*** is any set of forks and joins over
//! ***X***.  A _junction_ is any element of a flowset: a fork or a
//! join.
//!
//! * **Example 1.** The maximal flowset over the domain ***X*** =
//!   {<tt>_a_</tt>} is _&phi;_ =
//!   <tt>**{**(_a_,{_a_}),({_a_},_a_)**}**</tt>.
//!
//! Note, that given a domain ***X*** and a flowset _&phi;_ over
//! ***X***, all subsets of _&phi;_, including the empty set, are also
//! flowsets over ***X***.  Likewise, given any two flowsets over
//! ***X***, their union, intersection, set difference etc. are also
//! flowsets over ***X***.  Hence, for example, we may define three
//! unary flowset operators,
//!
//! > <tt>Fore</tt>(_&phi;_)&nbsp;=&nbsp;**{**(_x_, _w_) &in; _&phi;_ | _x_ &in; ***X*** &supe; _w_**}**,<br>
//! > <tt>Back</tt>(_&phi;_)&nbsp;=&nbsp;**{**(_w_, _x_) &in; _&phi;_ | _x_ &in; ***X*** &supe; _w_**}**, and<br>
//! > <tt>Flip</tt>(_&phi;_)&nbsp;=&nbsp;**{**(_x_, _w_) | (_w_, _x_) &in; <tt>Back</tt>(_&phi;_)**}** &cup; **{**(_w_, _x_) | (_x_, _w_) &in; <tt>Fore</tt>(_&phi;_)**}**.
//!
//! A flowset is _proper_ iff it has no loops, contains at least one
//! fork, and contains at least one join.
//!
//! * **Example 2.** The maximal proper flowset over the domain
//!   {<tt>_a_,_b_</tt>} is
//!   <tt>**{**(_a_,{_b_}),(_b_,{_a_}),({_b_},_a_),({_a_},_b_)**}**</tt>
//!   and the maximal proper flowset over the domain
//!   {<tt>_a_,_b_,_c_</tt>} is
//!   <tt>**{**(_a_,{_b_}),(_a_,{_c_}),(_a_,{_b_,_c_}),(_b_,{_a_}),(_b_,{_c_}),(_b_,{_a_,_c_}),(_c_,{_a_}),(_c_,{_b_}),(_c_,{_a_,_b_}),({_b_},_a_),({_c_},_a_),({_b_,_c_},_a_),({_a_},_b_),({_c_},_b_),({_a_,_c_},_b_),({_a_},_c_),({_b_},_c_),({_a_,_b_},_c_)**}**</tt>.
//!
//! Observe, looking at the last example, that the total number of
//! proper flowsets over a finite domain ***X*** is a double
//! exponential function of |***X***|.  Indeed, there are altogether
//! nine different proper flowsets over a 2-element domain and 261121
//! different proper flowsets over a 3-element domain.  In general, if
//! &mu;(***X***) denotes the maximal proper flowset over ***X***,
//! then |&mu;(***X***)| = 2 |***X***| (2<sup>|***X***| - 1</sup> -
//! 1), and the total number of proper flowsets is
//! (2<sup>|&mu;(***X***)|/2</sup> - 1)<sup>2</sup>.
//!
//! An _isomorphism_ between two flowsets _&phi;_ and _&psi;_ over
//! ***X*** is a bijection _m_ in ***X*** such that _(x, w)_ is in
//! <tt>Fore</tt>(_&phi;_) iff _(m(x), m(w))_ is in
//! <tt>Fore</tt>(_&psi;_), and symmetrically for
//! <tt>Back</tt>(_&phi;_) and <tt>Back</tt>(_&psi;_).  We write
//! _&phi;_ &cong; _&psi;_ to express that _&phi;_ and _&psi;_ are
//! isomorphic, and <tt>Aut</tt>(_&phi;_) &mdash; to denote the set of
//! all automorphisms of a flowset _&phi;_.
//!
//! * **Example 3.** There are six (five nontrivial) automorphisms of
//!   the flowset
//!   <tt>**{**(_a_,{_b_,_c_}),(_b_,{_a_,_c_}),(_c_,{_a_,_b_}),({_b_,_c_},_a_),({_a_,_c_},_b_),({_a_,_b_},_c_)**}**</tt>
//!   over the domain ***X*** = {<tt>_a_,_b_,_c_</tt>}.  These are the
//!   all bijections in ***X***, including (resp. excluding) the
//!   identity.
//!
//! * **Example 4.** There are no automorphisms of the flowset _&phi;_
//!   = <tt>**{**(_a_,{_b_,_c_}),({_a_,_b_},_c_)**}**</tt> over
//!   {<tt>_a_,_b_,_c_</tt>}, except for the trivial one.  The
//!   flowsets isomorphic to _&phi;_ are:
//!   <tt>**{**(_a_,{_b_,_c_}),({_a_,_c_},_b_)**}**</tt>,
//!   <tt>**{**(_b_,{_a_,_c_}),({_b_,_c_},_a_)**}**</tt>,
//!   <tt>**{**(_b_,{_a_,_c_}),({_a_,_b_},_c_)**}**</tt>,
//!   <tt>**{**(_c_,{_a_,_b_}),({_b_,_c_},_a_)**}**</tt>, and
//!   <tt>**{**(_c_,{_a_,_b_}),({_a_,_c_},_b_)**}**</tt>.
//!
//! FIXME count them up to isomorphism for |***X***| &leq; 4.
//!
//! ### Arming relations
//!
//! A flowset _&phi;_ may be &ldquo;compressed&rdquo; into an _arming_
//! relation between domain elements (nodes): node _y_ is _arming_
//! node _x_ in _&phi;_ (_x_ is _armed_ by _y_) iff _x_ is the eye of
//! some junction of _&phi;_ and _y_ is an arm of the same junction.
//! More specifically, there are two relations: a node _x_ may be
//! _fork-armed_ or _join-armed_ by node _y_, depending on whether _x_
//! is the eye of some fork or some join, and _y_ is an arm of the
//! same fork or join.  These two relations may be represented in a
//! symbolic notation:
//!
//! > _x_ &rarr;<sub>_&phi;_</sub> _y_, which denotes that _x_ is fork-armed by _y_ in _&phi;_,<br>
//! > _x_ &larr;<sub>_&phi;_</sub> _y_, which denotes that _x_ is join-armed by _y_ in _&phi;_,
//!
//! and their complements are
//!
//! > _x_ &nrarr;<sub>_&phi;_</sub> _y_, which holds if _x_ isn't fork-armed by _y_ in _&phi;_, and<br>
//! > _x_ &nlarr;<sub>_&phi;_</sub> _y_, which holds if _x_ isn't join-armed by _y_ in _&phi;_.
//!
//! The subscript may be omitted, whenever it is clear what flowset
//! happens to be under consideration.
//!
//! Armings may be a source of derived relations.  One possibility is
//! to lift arming domain from nodes to sets of nodes.  Then, a subset
//! ***Y*** of ***X*** is said to be fork-armed (join-armed) by a node
//! _x_ in a flowset _&phi;_ over ***X*** iff every element of ***Y***
//! is fork-armed (join-armed) by _x_ in _&phi;_.
//!
//! Another derivation is the transitive closure.  Given a flowset
//! _&phi;_, a node _y_ is _fork-connected_ (_join-connected_) to a
//! node _x_ in _&phi;_ iff _y_ is fork-arming (join-arming) the node
//! _x_ or some other node which is fork-connected (join-connected) to
//! _x_ in _&phi;_.  Symbolically,
//!
//! > _x_ &Rarr;<sub>_&phi;_</sub> _y_ holds if _y_ is fork-connected to _x_ in _&phi;_, and<br>
//! > _x_ &Larr;<sub>_&phi;_</sub> _y_ holds if _y_ is join-connected to _x_ in _&phi;_.
//!
//! Connections my be lifted in a similar way to armings: a subset
//! ***Y*** of ***X*** is fork-connected (join-connected) to a node
//! _x_ in a flowset _&phi;_ over ***X*** iff every element of ***Y***
//! is fork-connected (join-connected) to _x_ in _&phi;_.
//!
//! ### FIXME push-pull derivation
//!
//! FIXME broken arm.
//!
//! ### Classification of domain elements
//!
//! The _pre-set_ (_post-set_) of a flowset _&phi;_ is the set of eyes
//! of all forks (joins) in _&phi;_.  The _carrier_ of a flowset is
//! the set of all eyes, i.e. the union of its pre-set and post-set,
//! and the _interior_ is the intersection.
//!
//! The _under-set_ (_over-set_) of a flowset _&phi;_ is the union of
//! armsets of all forks (joins) in _&phi;_.  The _range_ of a flowset
//! is the set of all its arms, i.e. the union of all armsets (the
//! union of under-set and over-set), and the _frame_ &mdash; the set
//! of all armsets.  The _co-interior_ is the intersection of
//! under-set and over-set.  Finally, the _residue_ contains all
//! non-carrier elements of the range.
//!
//! The four basic classes may be defined symbolically,
//!
//! > <tt>Pre</tt>(_&phi;_)<tt>&nbsp;&nbsp;</tt>&nbsp;=&nbsp;{_x_ &in; ***X*** | &exist;<sub>_y_</sub>&nbsp;_x_ &rarr;<sub>_&phi;_</sub> _y_},<br>
//! > <tt>Post</tt>(_&phi;_)<tt>&nbsp;</tt>&nbsp;=&nbsp;{_x_ &in; ***X*** | &exist;<sub>_y_</sub>&nbsp;_x_ &larr;<sub>_&phi;_</sub> _y_},<br>
//! > <tt>Under</tt>(_&phi;_)&nbsp;=&nbsp;{_x_ &in; ***X*** | &exist;<sub>_y_</sub>&nbsp;_y_ &rarr;<sub>_&phi;_</sub> _x_},<br>
//! > <tt>Over</tt>(_&phi;_)<tt>&nbsp;</tt>&nbsp;=&nbsp;{_x_ &in; ***X*** | &exist;<sub>_y_</sub>&nbsp;_y_ &larr;<sub>_&phi;_</sub> _x_},
//!
//! and similarly,
//!
//! > <tt>Range</tt>(_&phi;_)<tt>&nbsp;</tt>&nbsp;=&nbsp;<tt>Over</tt>(_&phi;_) &cup; <tt>Under</tt>(_&phi;_)&nbsp;=&nbsp;{_x_ &in; ***X*** | &exist;<sub>_y_</sub>&nbsp;_y_ &larr;<sub>_&phi;_</sub> _x_ &or; _y_ &rarr;<sub>_&phi;_</sub> _x_}, and<br>
//! > <tt>Frame</tt>(_&phi;_)&nbsp;=&nbsp;{_w_ &subset; ***X*** | &exist;<sub>_x_</sub>&nbsp;(_x_, _w_) &in; _&phi;_ &or; (_w_, _x_) &in; _&phi;_}.
//!
//! Altogether, any flowset generates a partition of the domain into
//! some number (up to 23) of disjoint classes listed in the following
//! _domain partition table_.
//!
//! | <p align="right">_arm_</p>_eye_ | <center>not an arm<br>(not in range)</center> | <center>join's arm<br>(in over-set)</center> | <center>fork's arm<br>(in under-set)</center> | <center>both<br>(in co-interior)</center> |
//! |-----------------------------------------|:------------:|:--------------:|:--------------:|:-------------:|
//! | **not an eye**<br>**(not in carrier)**  | **isolated** | upper residual | lower residual | full residual |
//! | **fork's eye**<br>**(in pre-set)**  | weak<br>source | broken or **strong**<br>**source** | weak<br>pseudo-source | broken or strong<br>pseudo-source |
//! | **join's eye**<br>**(in post-set)** | weak<br>sink | weak<br>pseudo-sink | broken or **strong**<br>**sink** | broken or strong<br>pseudo-sink |
//! | **both**<br>**(in interior)**           | weak<br>internal | broken or strong<br>upper internal | broken or strong<br>lower internal | broken or **strong**<br>full **internal** |
//!
//! In particular, a _source_ of a flowset is a pre-set node that is
//! neither in the post-set nor in the under-set, and a _sink_ &mdash;
//! a post-set node that is neither in the pre-set nor in the
//! over-set.  A source, a sink, or an internal node is _weak_, if it
//! is not in the range.  A pre-set element that is in the under-set,
//! but not in the interior, is a _pseudo-source_, and a post-set
//! element that is in the over-set, but not in the interior &mdash; a
//! _pseudo-sink_.  A pseudo-source or pseudo-sink is _weak_, if it is
//! not in the co-interior.
//!
//! Seven cells of the table are split into &ldquo;broken&rdquo; and
//! &ldquo;strong&rdquo; variants.  A carrier node _x_ is _broken_ iff
//! there are two nodes such that _x_ is join-armed (fork-armed) by
//! each, but exactly one of them is fork-armed (join-armed) by _x_.
//! A carrier element is _strong_ iff it is neither weak nor broken.
//!
//! The formulae for a strong and for a broken node _x_ may be written
//! as
//!
//! > <tt>IsStrong</tt>(_x_)&nbsp;&equiv;&nbsp;&forall;<sub>_y_</sub>&nbsp; (_x_ &rarr; _y_ &hArr; _y_ &larr; _x_) &nbsp;&and;&nbsp; (_x_ &larr; _y_ &hArr; _y_ &rarr; _x_),<br>
//! > <tt>IsBroken</tt>(_x_)&nbsp;&equiv;&nbsp;(&exist;<sub>_y_,_z_</sub>&nbsp; _x_ &rarr; _y_ &and; _z_ &larr; _x_ &and; _y_ &nlarr; _x_)&nbsp;
//! > &or; &nbsp;(&exist;<sub>_y_,_z_</sub>&nbsp; _x_ &larr; _y_ &and; _z_ &rarr; _x_ &and; _y_ &nrarr; _x_).
//!
//! A node is _isolated_ in a flowset _&phi;_ iff it is neither in the
//! carrier nor in the residue of _&phi;_.  We say, that any
//! non-isolated node _occurs_ in _&phi;_.
//!
//! ### Classification of flowsets
//!
//! A flowset is _flat_ iff its interior is empty, i.e. if its pre-set
//! and post-set are disjoint.  A _diode_ is a 2-element flat proper
//! flowset.
//!
//! FIXME _primitive_ flowsets.
//!
//! A _star_ is a flat flowset with a singleton carrier.  All stars
//! are improper (note, that singleton-carrier flowsets are either
//! proper or flat, but not both).  If a star _&alpha;_ is a subset of
//! a flowset _&phi;_, then we say that it is a _star of_ _x_ _in_
//! _&phi;_, where _x_ is the only carrier node.  A flowset _&phi;_ is
//! _singular_ iff all stars in _&phi;_ are singletons.  The _star
//! partition_ of a flowset _&phi;_ is the set of all maximal stars in
//! _&phi;_.
//!
//! Given a flowset _&phi;_ and a node _x_, the maximal
//! fork-containing star of _x_ in _&phi;_ is called the _lower star_
//! of _x_ in _&phi;_ and is denoted by _x_<sub>&star;</sub>(_&phi;_),
//! or simply by _x_<sub>&star;</sub>.  Similarly,
//! _x_<sup>&star;</sup>(_&phi;_) denotes the _upper star_ of _x_ in
//! _&phi;_ &mdash; the maximal join-containing star of _x_ in
//! _&phi;_.  Of course, the sets <tt>Pre</tt>(_x_<sup>&star;</sup>),
//! <tt>Post</tt>(_x_<sub>&star;</sub>),
//! <tt>Under</tt>(_x_<sup>&star;</sup>), and
//! <tt>Over</tt>(_x_<sub>&star;</sub>) are all empty for any node _x_
//! in any flowset.
//!
//! FIXME gazers.
//!
//! The maximal set of forks (joins) armed by a node _x_ is called the
//! _pre-gazer_ (_post-gazer_) of _x_.  Symbolically,
//!
//! > <sup>&oast;</tt></sup>_x_(_&phi;_) denotes the pre-gazer of _x_ in _&phi;_, and<br>
//! > _x_<sup>&oast;</sup>(_&phi;_) denotes the post-gazer of _x_ in _&phi;_.
//!
//! A flowset _&phi;_ is _symmetric_ iff it is isomorphic to its flip:
//! _&phi;_ &cong; <tt>Flip</tt>(_&phi;_).
//!
//! A flowset is _coherent_ iff the only nodes occuring in it are
//! strong sources, strong sinks, and strong internals.  Equivalently,
//! a flowset is coherent iff it generates a domain partition
//! containing no more than the four classes listed in bold face in
//! the main diagonal of the domain partition table.
//!
//! A flowset is _tight_ iff any fork's eye is every join's arm and
//! any join's eye is every fork's arm.
//!
//! * **Example 5.** The flowset
//!   <tt>**{**(_a_,{_x_,_y_}),({_a_},_x_),({_a_},_y_)**}**</tt> is
//!   coherent and tight.  It is the union of two incoherent minimal
//!   tight flowsets: <tt>**{**(_a_,{_x_,_y_}),({_a_},_x_)**}**</tt>
//!   and <tt>**{**(_a_,{_x_,_y_}),({_a_},_y_)**}**</tt>.
//!
//! * **Example 6.** The flowset
//!   <tt>**{**(_a_,{_x_}),(_b_,{_x_,_y_}),(_c_,{_y_}),({_a_,_b_},_x_),({_b_,_c_},_y_)**}**</tt>
//!   is coherent but not tight.
//!
//! ### Fork-join hypergraphs
//!
//! It is natural to perceive singular flowsets and primitive flowsets
//! as graph-like structures.  This correspondence may suggest an
//! alternative description of flowsets and hints to this possibility
//! are in some of the names already introduced, like _node_, _loop_,
//! _star_, etc.
//!
//! Instead of defining forks and joins over a fixed domain common to
//! all flowsets, one may want to replace the notion of a flowset
//! with a variant of labeled directed hypergraphs, and define forks
//! and joins over node sets of individual hypergraphs.
//!
//! A _fork-join hypergraph_ may thus be defined as a directed
//! hypergraph with a hyperedge-labeling function separating forks
//! from joins, and satisfying the property, that all forks are
//! singleton-tail hyperedges (F-edges) and all joins are
//! singleton-head hyperedges (B-edges).  In this setting, for
//! example, under-set is the neighborhood of the pre-set, over-set is
//! the neighborhood of the post-set, and range is the neighborhood of
//! the carrier.
//!
//! # Tightness
//!
//! All subsets of any tight flowset are tight.  Therefore, given any
//! tight flowset _&phi;_ and any flowset _&psi;_, their intersection
//! and set difference, _&phi;_ &setminus; _&psi;_ are tight
//! (absorption).  FIXME However, their union may not be tight (no
//! closure).
//!
//! FIXME All proper tight flowsets are flat.
//!
//! FIXME Minimal tight union of maximal stars in a flowset _&phi;_.
//!
//! FIXME Two lower (upper) stars are _colliding_ iff their frames
//! overlap: the frames are intersecting, yet incomparable by
//! inclusion.
//!
//! FIXME Maximal tight subset of a flowset _&phi;_.
//!
//! ### Tightness and graphs
//!
//! FIXME ties and t-graphs: bipartite graph linking tied junctions.
//!
//! FIXME d-graphs linking tight diodes having at least one comparable
//! junction.
//!
//! # Coherence
//!
//! Given any two coherent flowsets, their union is a coherent
//! flowset.  FIXME However, neither intersection, nor set difference
//! has to be.
//!
//! Given a flowset _&phi;_ over ***X*** and two elements _x_ and _y_
//! of _X, we say_ that
//!
//! * _y_ _weakly follows_ _x_ iff _y_ is join-armed by _x_ or _x_ is
//!   fork-armed by _y_, but not both; synonymously, _y_ is a _weak
//!   follower_ of _x_;
//!
//! * _y_ _strongly follows_ _x_ iff _y_ is join-armed by _x_ and _x_
//!   is fork-armed by _y_; synonymously, _y_ is a _strong follower_
//!   of _x_.
//!
//! As may be shown by examination of the domain partition table, the
//! exclusion of weak followers is equivalent to coherence.
//!
//! A _strong patching_ of a node _x_ in a flowset _&phi;_ is a pair
//! of domain subsets (<tt><b>P</b></tt><sub>_&phi;_</sub>(_x_),
//! <tt><b>p</b></tt><sub>_&phi;_</sub>(_x_)), where the components
//! <tt><b>P</b></tt><sub>_&phi;_</sub>(_x_)&nbsp;=&nbsp;{_y_ | _x_ &larr;<sub>_&phi;_</sub> _y_ &and; _y_ &rarr;<sub>_&phi;_</sub> _x_} and
//! <tt><b>p</b></tt><sub>_&phi;_</sub>(_x_)&nbsp;=&nbsp;{_y_ | _x_ &rarr;<sub>_&phi;_</sub> _y_ &and; _y_ &larr;<sub>_&phi;_</sub> _x_} are the _upper_ and _lower strong patching_ of _x_ in _&phi;_.
//!
//! A _weak patching_ of a node _x_ in a flowset _&phi;_ is a 4-tuple
//! of domain subsets (<tt><b>A</b></tt><sub>_&phi;_</sub>(_x_),
//! <tt><b>a</b></tt><sub>_&phi;_</sub>(_x_),
//! <tt><b>E</b></tt><sub>_&phi;_</sub>(_x_), <tt><b>e</b></tt><sub>_&phi;_</sub>(_x_)), where the components
//! <tt><b>A</b></tt><sub>_&phi;_</sub>(_x_)&nbsp;=&nbsp;{_y_ | _x_ &larr;<sub>_&phi;_</sub> _y_ &and; _y_ &nrarr;<sub>_&phi;_</sub> _x_},
//! <tt><b>a</b></tt><sub>_&phi;_</sub>(_x_)&nbsp;=&nbsp;{_y_ | _x_ &rarr;<sub>_&phi;_</sub> _y_ &and; _y_ &nlarr;<sub>_&phi;_</sub> _x_},
//! <tt><b>E</b></tt><sub>_&phi;_</sub>(_x_)&nbsp;=&nbsp;{_y_ | _y_ &rarr;<sub>_&phi;_</sub> _x_ &and; _x_ &nlarr;<sub>_&phi;_</sub> _y_}, and
//! <tt><b>e</b></tt><sub>_&phi;_</sub>(_x_)&nbsp;=&nbsp;{_y_ | _y_ &larr;<sub>_&phi;_</sub> _x_ &and; _x_ &nrarr;<sub>_&phi;_</sub> _y_},
//! are the _upper_ and _lower arm patching_ of _x_ in _&phi;_ (the
//! nodes arming _x_), and the _upper_ and _lower eye patching_ of _x_
//! in _&phi;_ (the eyes armed by _x_).
//!
//! The following _weak patching pattern table_ lists possible
//! component patterns of weak patching of a node, depending
//! on its class.  Non-empty components are represented by the
//! corresponding letters: <tt><b>A</b></tt>, <tt><b>a</b></tt>,
//! <tt><b>E</b></tt>, and <tt><b>e</b></tt>.  Empty components are
//! omitted, unless all four components are empty, in which case the
//! pattern is represented by the symbol <tt>&empty;</tt>.
//!
//! | <p align="right">_arm_</p>_eye_ | <center>not an arm<br>(not in range)</center> | <center>join's arm<br>(in over-set)</center> | <center>fork's arm<br>(in under-set)</center> | <center>both<br>(in co-interior)</center> |
//! |-----------------------------------------|:----:|:----------------------:|:----------------------:|:----------------------:|
//! | **not an eye**<br>**(not in carrier)**  | <tt>&empty;</tt> | <tt><b>e</b></tt> | <tt><b>E</b></tt> | <tt><b>Ee</b></tt> |
//! | **fork's eye**<br>**(in pre-set)**  | <tt><b>a</b></tt> | <tt><b>ae</b></tt>, <tt><b>a</b></tt>, <tt><b>e</b></tt>, <tt>&empty;</tt> | <tt><b>aE</b></tt> | <tt><b>aEe</b></tt>, <tt><b>aE</b></tt>, <tt><b>Ee</b></tt>, <tt><b>E</b></tt> |
//! | **join's eye**<br>**(in post-set)** | <tt><b>A</b></tt> | <tt><b>Ae</b></tt> | <tt><b>AE</b></tt>, <tt><b>A</b></tt>, <tt><b>E</b></tt>, <tt>&empty;</tt> | <tt><b>AEe</b></tt>, <tt><b>Ae</b></tt>, <tt><b>Ee</b></tt>, <tt><b>e</b></tt> |
//! | **both**<br>**(in interior)**           | <tt><b>Aa</b></tt> | <tt><b>Aae</b></tt>, <tt><b>Aa</b></tt>, <tt><b>Ae</b></tt>, <tt><b>A</b></tt> | <tt><b>AaE</b></tt>, <tt><b>Aa</b></tt>, <tt><b>aE</b></tt>, <tt><b>a</b></tt> | any pattern |
//!
//! ### Set equality condition
//!
//! For a flowset to be coherent, it is necessary that its pre-set be
//! equal to over-set and its post-set be equal to under-set.  This is
//! the _set equality condition_ of coherence.
//!
//! To see that this is the case, note that if a pre-set node _x_
//! isn't in the over-set, then _x_ is fork-armed by at least one arm
//! and all such arms are weak followers of _x_; conversely, if an
//! over-set node _x_ isn't in the pre-set, then there is at least one
//! eye join-armed by _x_ and all such eyes are weak followers of _x_.
//! Hence, if a flowset doesn't contain weak followers, then its
//! pre-set and over-set must be equal.  The other equality is
//! supported by the symmetric argument.
//!
//! However, set equality isn't a sufficient condition of strength.
//! For example, it is possible to find a coherent flowset _&phi;_
//! over some domain ***X***, and a fork (_x_, _w_) over ***X***, not
//! a member of _&phi;_, such that _x_ is in <tt>Pre</tt>(_&phi;_),
//! _w_ is a subset of <tt>Under</tt>(_&phi;_), but not a subset of
//! <tt>Under</tt>(_x_<sub>&star;</sub>(_&phi;_)).  Then, if _&phi;_
//! is augmented with such a fork to form a flowset _&psi;_, the set
//! equality condition is satisfied by _&psi;_, but _&psi;_ isn't
//! coherent, because any element of _w_ not in
//! <tt>Under</tt>(_x_<sub>&star;</sub>(_&phi;_)) is a weak follower
//! of _x_ in _&psi;_.
//!
//! # Product flowsets
//!
//! FIXME parallel multiplication.
//!
//! # Flowset profiles
//!
//! FIXME additive profile, multiplicative profile
//!
//! # Flow paths and tracks
//!
//! ### Flow paths
//!
//! Given elements _s_ and _t_ of ***X***, a _flow path_ _&pi;_ from _s_
//! to _t_ is a minimal non-empty flowset over ***X***, such that the
//! post-set of _&pi;_ is fork-connected to _s_ in _&pi;_ and the
//! pre-set of _&pi;_ is join-connected to _t_ in _&pi;_.
//!
//! > _s_ &Rarr;<sub>_&pi;_</sub> <tt>Post</tt>(_&pi;_) &and; _t_ &Larr;<sub>_&pi;_</sub> <tt>Pre</tt>(_&pi;_)
//!
//! A flowset _&phi;_ over ***X*** forms a _flow cycle_ iff there is an _x_
//! in ***X*** such that _&phi;_ is a flow path from _x_ to _x_.
//!
//! FIXME Given a path _&pi;<sub>1</sub>_ from _s_ to _r_ and a path
//! _&pi;<sub>2</sub>_ from _r_ to _t_
//!
//! FIXME If a node _r_ is internal in a path _&pi;_ from _s_ to _t_
//!
//! FIXME all flow paths are singular.
//!
//! FIXME a flowset is a flat path iff it is a minimal tight flowset.
//! If a flow path isn't flat, then it is not tight.
//!
//! FIXME It is convenient to say _flat path_ instead of _flat flow
//! path_.
//!
//! ### Stems
//!
//! Given a flowset _&phi;_ over ***X***, and two subsets ***Y***,
//! ***Z*** of ***X***, ***Z*** is _hyper-connected_ to ***Y*** in
//! _&phi;_ iff any element of ***Z*** is fork-connected to some
//! element of ***Y*** and any element of ***Y*** is join-connected to
//! some element of ***Z***.
//!
//! Given subsets ***S*** and ***T*** of ***X***, a _stem_ from
//! ***S*** to ***T*** is a flowset _&sigma;_ over ***X*** such that
//! the post-set of _&sigma;_ is hyper-connected to ***S*** in
//! _&sigma;_ and the pre-set of _&sigma;_ is hyper-connected to
//! ***T*** in _&sigma;_.  A stem is _cycle-free_ iff it doesn't
//! contain any flow cycles.
//!
//! FIXME Note, that all singular coherent stems are minimal, i.e. one
//! such stem never includes another as a proper subset.
//!
//! FIXME A singular coherent flat stem is called a _core flower_.
//!
//! FIXME not all flowers are tight.
//!
//! # Core net
//!
//! The set <tt>Core</tt>(***X***) of all core flowers over some
//! domain ***X*** will be called the _core_ of ***X***.  Similarly,
//! the set <tt>Core</tt>(_&phi;_) of all core flowers included in a
//! flowset _&phi;_ will be called the _core_ of _&phi;_.
//!
//! The _core net_ of a domain ***X*** is the bipartite digraph
//! linking an element _x_ of ***X*** to a core flower _&gamma;_ in
//! <tt>Core</tt>(***X***) iff _x_ is in the pre-set of _&gamma;_, and
//! linking a core flower _&delta;_ of <tt>Core</tt>(***X***) to an
//! element _y_ of ***X*** iff _y_ is in the post-set of _&delta;_.
//! The _carrier_ of the core net of ***X*** is the union of carriers
//! of all core flowers in <tt>Core</tt>(***X***) &mdash; the set of
//! all elements of ***X*** which aren't isolated in the graph.  The
//! _range_ of a core net is the neighborhood of its carrier.
//!
//! The core net of a flowset _&phi;_ over ***X*** is the restriction
//! of the core net of ***X*** to the core of _&phi;_, i.e. the
//! subgraph induced by the union of ***X*** and
//! <tt>Core</tt>(_&phi;_).
//!
//! FIXME reachability, transoid

#![feature(bool_to_option)]
#![allow(clippy::toplevel_ref_arg)]

#[macro_use]
extern crate log;
#[macro_use]
extern crate lazy_static;

mod error;
mod name;
mod node;
mod atom;
mod multiset;
mod polynomial;
mod ces;
mod context;
mod content;
mod yaml_script;
pub mod sat;
mod solver;
mod state;
mod firing;
mod runner;
mod vis;
mod logging;

pub use error::{AcesError, AcesErrorKind};
pub use context::{Context, ContextHandle, Contextual, ExclusivelyContextual, InContext, InContextMut};
pub use content::{
    Content, ContentFormat, InteractiveFormat, PartialContent, Compilable, CompilableMut,
    CompilableAsContent, CompilableAsDependency,
};
pub use yaml_script::YamlFormat;
pub use node::{NodeId, Face};
pub use atom::{
    Port, Link, Harc, Fork, Join, FlowSet, AtomId, PortId, LinkId, ForkId, JoinId, FlowSetId,
    Atomic,
};
pub use polynomial::{Polynomial, Monomials};
pub use multiset::{Multiplicity, Capacity, Weight, CoreWeight, ShellWeight};
pub use ces::CEStructure;
pub use solver::{Solver, Solution};
pub use firing::{FiringComponent, FiringSet, FiringSequence};
pub use state::{State, Goal, Semantics};
pub use runner::{Runner, StopCondition};
pub use logging::Logger;

use std::num::NonZeroUsize;

/// A generic one-based serial identifier.
///
/// Used as a common internal type backing the conversion between
/// vector indices and specific identifiers, such as [`NodeId`],
/// [`PortId`], [`LinkId`], [`ForkId`], and [`JoinId`].
pub(crate) type AnyId = NonZeroUsize;

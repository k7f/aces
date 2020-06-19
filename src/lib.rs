//! [Algebra of Cause-Effect
//! Structures](https://link.springer.com/book/10.1007/978-3-030-20461-7)
//! &mdash; an implementation of the theory.  This is the core library
//! of the [_Ascesis_](https://docs.rs/ascesis) project.
//!
//! In an attempt to clarify some of the obscure parts of the library,
//! several definitions are collected below to serve as a reference
//! for implementation.
//!
//! These notes are, in a way, a draft of a &ldquo;low level&rdquo;
//! reformulation of the theory, not as concise and elegant as the
//! original but, hopefully, better suited for the design and analysis
//! of implementation details.  By describing elementary c-e
//! structures in a slightly more general context &mdash;
//! characterising them as a special case of _fusion sets_ &mdash; one
//! may expect to better understand, for instance, the algorithms of
//! structural decomposition and search.
//!
//! # Fusets
//!
//! Consider a _domain_ ***X*** &mdash; a non-empty and countable set
//! of _points_ (variables).  A _fork_ over ***X*** is any pair _(x,
//! u)_, and a _join_ is any pair _(u, x)_, where _x_ is a point from
//! ***X***, called a _tip_ of a fork (or join), and _u_ is a
//! non-empty subset of ***X***, called a _pot_ of a fork (or join).
//! We call pot members _arms_ and say that any arm of a fork (join)
//! is _arming_ both the fork (join) and its tip.  We also say that a
//! fork _(x, u)_ or join _(u, x)_ is _tipped_ by _x_ or that it is
//! _x-tipped_.  A fork (join) is a _loop_ iff its tip is in its pot.
//!
//! A _fusion set_ over ***X*** is any set of forks and joins over
//! ***X***.  The term is almost always shortened to _fuset_ in text
//! and &ldquo;few-set&rdquo; in sound.  A _wide edge_ (_wedge_) is
//! any element of a fuset: a fork or a join.  _Width_ of a wedge is
//! the number of its arms.
//!
//! * **Example 1.** The maximal fuset over the domain ***X*** =
//!   {<tt>_a_</tt>} is _&phi;_ =
//!   <tt>**{**(_a_,{_a_}),({_a_},_a_)**}**</tt>.
//!
//! Note, that given a domain ***X*** and a fuset _&phi;_ over
//! ***X***, all subsets of _&phi;_, including the empty set, are also
//! fusets over ***X***.  Likewise, given any two fusets over ***X***,
//! their union, intersection, set difference etc. are all fusets over
//! ***X***.  Hence, for example, we may define three unary fuset
//! operators,
//!
//! > <tt>Fore</tt>(_&phi;_)&nbsp;=&nbsp;**{**(_x_, _u_) &in; _&phi;_ | _x_ &in; ***X*** &supe; _u_**}**,<br>
//! > <tt>Back</tt>(_&phi;_)&nbsp;=&nbsp;**{**(_u_, _x_) &in; _&phi;_ | _x_ &in; ***X*** &supe; _u_**}**, and<br>
//! > <tt>Flip</tt>(_&phi;_)&nbsp;=&nbsp;**{**(_x_, _u_) | (_u_, _x_) &in; <tt>Back</tt>(_&phi;_)**}** &cup; **{**(_u_, _x_) | (_x_, _u_) &in; <tt>Fore</tt>(_&phi;_)**}**.
//!
//! A fuset is _proper_ iff it has no loops, contains at least one
//! fork, and contains at least one join.
//!
//! * **Example 2.** The maximal proper fuset over the domain
//!   {<tt>_a_,_b_</tt>} is
//!   <tt>**{**(_a_,{_b_}),(_b_,{_a_}),({_b_},_a_),({_a_},_b_)**}**</tt>
//!   and the maximal proper fuset over the domain
//!   {<tt>_a_,_b_,_c_</tt>} is
//!   <tt>**{**(_a_,{_b_}),(_a_,{_c_}),(_a_,{_b_,_c_}),(_b_,{_a_}),(_b_,{_c_}),(_b_,{_a_,_c_}),(_c_,{_a_}),(_c_,{_b_}),(_c_,{_a_,_b_}),({_b_},_a_),({_c_},_a_),({_b_,_c_},_a_),({_a_},_b_),({_c_},_b_),({_a_,_c_},_b_),({_a_},_c_),({_b_},_c_),({_a_,_b_},_c_)**}**</tt>.
//!
//! Observe, looking at the last example, that the total number of
//! proper fusets over a finite domain ***X*** is a double exponential
//! function of |***X***|.  Indeed, there are altogether nine
//! different proper fusets over a 2-element domain and 261121
//! different proper fusets over a 3-element domain.  In general, if
//! &mu;(***X***) denotes the maximal proper fuset over ***X***, then
//! |&mu;(***X***)| = 2 |***X***| (2<sup>|***X***| - 1</sup> - 1), and
//! the total number of proper fusets is
//! (2<sup>|&mu;(***X***)|/2</sup> - 1)<sup>2</sup>.
//!
//! An _isomorphism_ between two fusets _&phi;_ and _&psi;_ over
//! ***X*** is a bijection _m_ in ***X*** such that _(x, u)_ is in
//! <tt>Fore</tt>(_&phi;_) iff _(m(x), m(u))_ is in
//! <tt>Fore</tt>(_&psi;_), and symmetrically for
//! <tt>Back</tt>(_&phi;_) and <tt>Back</tt>(_&psi;_).  We write
//! _&phi;_ &cong; _&psi;_ to express that _&phi;_ and _&psi;_ are
//! isomorphic, and <tt>Aut</tt>(_&phi;_) &mdash; to denote the set of
//! all automorphisms of a fuset _&phi;_.
//!
//! * **Example 3.** There are six (five nontrivial) automorphisms of
//!   the fuset _&phi;_ =
//!   <tt>**{**(_a_,{_b_,_c_}),(_b_,{_a_,_c_}),(_c_,{_a_,_b_}),({_b_,_c_},_a_),({_a_,_c_},_b_),({_a_,_b_},_c_)**}**</tt>
//!   over the domain ***X*** = {<tt>_a_,_b_,_c_</tt>}.  These are the
//!   all bijections in ***X***, including (resp. excluding) the
//!   identity.  No other fuset over ***X*** is isomorphic to _&phi;_.
//!
//! * **Example 4.** There are no automorphisms of the fuset _&phi;_ =
//!   <tt>**{**(_a_,{_b_,_c_}),({_a_,_b_},_c_)**}**</tt> over ***X***
//!   = {<tt>_a_,_b_,_c_</tt>}, except for the trivial one, and there
//!   are five other fusets over ***X*** that are isomorphic to
//!   _&phi;_: <tt>**{**(_a_,{_b_,_c_}),({_a_,_c_},_b_)**}**</tt>,
//!   <tt>**{**(_b_,{_a_,_c_}),({_b_,_c_},_a_)**}**</tt>,
//!   <tt>**{**(_b_,{_a_,_c_}),({_a_,_b_},_c_)**}**</tt>,
//!   <tt>**{**(_c_,{_a_,_b_}),({_b_,_c_},_a_)**}**</tt>, and
//!   <tt>**{**(_c_,{_a_,_b_}),({_a_,_c_},_b_)**}**</tt>.
//!
//! FIXME count them up to isomorphism for |***X***| &leq; 4.
//!
//! The _pre-set_ (_post-set_) of a fuset _&phi;_ is the set of tips
//! of all forks (joins) in _&phi;_.  The _carrier_ of a fuset is the
//! set of all tips, i.e. the union of its pre-set and post-set, and
//! the _interior_ is the intersection.
//!
//! The _under-set_ (_over-set_) of a fuset _&phi;_ is the union of
//! pots of all forks (joins) in _&phi;_.  The _range_ of a fuset is
//! the set of all its arms, i.e. the union of all pots (the union of
//! under-set and over-set), and the _frame_ &mdash; the set of all
//! pots.  The _co-interior_ is the intersection of under-set and
//! over-set.  Finally, the _residue_ contains all non-carrier
//! elements of the range.
//!
//! ### An alternative: fork-join hypergraphs
//!
//! It is natural to perceive fusion sets as graph-like structures.
//! This correspondence may suggest an alternative description in
//! terms of the more standard theory of hypergraphs: instead of
//! defining forks and joins over a fixed domain common to all
//! structures, one may want to define forks and joins over node sets
//! of individual signed directed hypergraphs.
//!
//! A _fork-join hypergraph_ may thus be defined as a directed
//! hypergraph equipped with a labeling function partitioning
//! hyperedges into forks and joins, such that all forks are
//! singleton-tail hyperedges (F-edges) and all joins are
//! singleton-head hyperedges (B-edges).  In this setting, for
//! example, under-set is the neighborhood of the pre-set, over-set is
//! the neighborhood of the post-set, and range is the neighborhood of
//! the carrier.
//!
//! The two concepts are compared in the table below.
//!
//! | <center>fork-join hypergraph</center>        | <center>fusion set</center>
//! |----------------------------------------------|-|
//! | a tripple: nodeset, hedgeset, polarity map   | just a set of wedges
//! | tail and head of a hedge are sets of nodes   | wedge binds a point and a set of points
//! | polarity is an attribute                     | forks and joins are structurally different
//! | either tail or head must be a singleton      | not a hypergraph
//! | fusing: lifting of the above restriction     | fusion: transformation into a directed hypergraph
//!
//! We will preserve the standard (and unrestricted) notion of a
//! _directed hypergraph_ and use the term when analysing reachability
//! properties of fusion sets.
//!
//! ### Arming and framing relations
//!
//! A fuset _&phi;_ may be &ldquo;compressed&rdquo; into an _arming_
//! relation between domain elements (points).  We say that a point
//! _x_ is _armed_ by a point _y_ in _&phi;_ (_y_ is _arming_ _x_)
//! when _x_ is the tip of some wedge of _&phi;_ and _y_ is an arm of
//! the same wedge.  More often we consider the two specific
//! relations: a point _x_ may be _fork-armed_ or _join-armed_ by
//! point _y_, depending on whether _x_ is the tip of some fork or
//! some join, and _y_ is an arm of the same fork or join.  These two
//! relations may be represented in a symbolic notation:
//!
//! > _x_ &rarr;<sub>_&phi;_</sub> _y_, which denotes that _x_ is fork-armed by _y_ in _&phi;_,<br>
//! > _x_ &larr;<sub>_&phi;_</sub> _y_, which denotes that _x_ is join-armed by _y_ in _&phi;_,
//!
//! and their complements are
//!
//! > _x_ &nrarr;<sub>_&phi;_</sub> _y_, which holds if _x_ isn't fork-armed by _y_ in _&phi;_, and<br>
//! > _x_ &nlarr;<sub>_&phi;_</sub> _y_, which holds if _x_ isn't join-armed by _y_ in _&phi;_.
//!
//! The subscript may be omitted, whenever it is clear what fuset
//! happens to be under consideration.
//!
//! It is also useful to introduce a stronger version of arming
//! relation, which holds when a point is in a way
//! &ldquo;maximally&rdquo; arming another point.  We say that a point
//! _x_ is _framed_ (or _fork-framed_, or _join-framed_) by a point
//! _y_ in a fuset _&phi;_ when _y_ is an arm of all the wedges (or
//! forks, or joins) in _&phi;_ which are tipped by _x_ and there is
//! at least one such wedge.  In other words, _x_ is framed
//! (fork-framed, join-framed) by _y_ iff the intersection of all pots
//! of _x_-tipped wedges (forks, joins) is non-empty and contains _y_.
//!
//! ### Classification of domain elements
//!
//! The already defined four basic point sets: pre-set, post-set,
//! undet-set and over-set, may be defined symbolically,
//!
//! > <tt>Pre</tt>(_&phi;_)<tt>&nbsp;&nbsp;</tt>&nbsp;=&nbsp;{_x_ &in; ***X*** | &exist;<sub>_y_</sub>&nbsp;_x_ &rarr;<sub>_&phi;_</sub> _y_},<br>
//! > <tt>Post</tt>(_&phi;_)<tt>&nbsp;</tt>&nbsp;=&nbsp;{_x_ &in; ***X*** | &exist;<sub>_y_</sub>&nbsp;_x_ &larr;<sub>_&phi;_</sub> _y_},<br>
//! > <tt>Under</tt>(_&phi;_)&nbsp;=&nbsp;{_x_ &in; ***X*** | &exist;<sub>_y_</sub>&nbsp;_y_ &rarr;<sub>_&phi;_</sub> _x_},<br>
//! > <tt>Over</tt>(_&phi;_)<tt>&nbsp;</tt>&nbsp;=&nbsp;{_x_ &in; ***X*** | &exist;<sub>_y_</sub>&nbsp;_y_ &larr;<sub>_&phi;_</sub> _x_},
//!
//! and similarly for range and frame,
//!
//! > <tt>Range</tt>(_&phi;_)<tt>&nbsp;</tt>&nbsp;=&nbsp;<tt>Over</tt>(_&phi;_) &cup; <tt>Under</tt>(_&phi;_)&nbsp;=&nbsp;{_x_ &in; ***X*** | &exist;<sub>_y_</sub>&nbsp;_y_ &larr;<sub>_&phi;_</sub> _x_ &or; _y_ &rarr;<sub>_&phi;_</sub> _x_}, and<br>
//! > <tt>Frame</tt>(_&phi;_)&nbsp;=&nbsp;{_u_ &subset; ***X*** | &exist;<sub>_x_</sub>&nbsp;(_x_, _u_) &in; _&phi;_ &or; (_u_, _x_) &in; _&phi;_}.
//!
//! Given a fuset _&phi;_, the _frame intersection_ is the set
//! &xcap;<tt>Frame</tt>(_&phi;_), the _upper frame intersection_ is
//! the frame intersection of <tt>Back</tt>(_&phi;_), and the _lower
//! frame intersection_ is the upper frame intersection of the flip.
//!
//! Altogether, any fuset generates a partition of the domain into
//! some number (up to 23) of disjoint classes listed in the following
//! _domain partition table_.
//!
//! | <p align="right">_arm_</p>_tip_ | <center>not an arm<br>(not in range)</center> | <center>join's arm<br>(in over-set)</center> | <center>fork's arm<br>(in under-set)</center> | <center>both<br>(in co-interior)</center> |
//! |-----------------------------------------|:------------:|:--------------:|:--------------:|:-------------:|
//! | **not a tip**<br>**(not in carrier)**  | **isolated** | upper residual | lower residual | full residual |
//! | **fork's tip**<br>**(in pre-set)**  | weak<br>source | broken or **strong**<br>**source** | weak<br>pseudo-source | broken or strong<br>pseudo-source |
//! | **join's tip**<br>**(in post-set)** | weak<br>sink | weak<br>pseudo-sink | broken or **strong**<br>**sink** | broken or strong<br>pseudo-sink |
//! | **both**<br>**(in interior)**           | weak<br>internal | broken or strong<br>upper internal | broken or strong<br>lower internal | broken or **strong**<br>full **internal** |
//!
//! In particular, a _source_ of a fuset is a pre-set point that is
//! neither in the post-set nor in the under-set, and a _sink_ &mdash;
//! a post-set point that is neither in the pre-set nor in the
//! over-set.  A source, a sink, or an internal point is _weak_, if it
//! is not in the range.  A pre-set element that is in the under-set,
//! but not in the interior, is a _pseudo-source_, and a post-set
//! element that is in the over-set, but not in the interior &mdash; a
//! _pseudo-sink_.  A pseudo-source or pseudo-sink is _weak_, if it is
//! not in the co-interior.
//!
//! Seven cells of the table are split into &ldquo;broken&rdquo; and
//! &ldquo;strong&rdquo; variants.  A carrier point _x_ is _broken_ iff
//! there are two points such that _x_ is join-armed (fork-armed) by
//! each, but exactly one of them is fork-armed (join-armed) by _x_.
//! A carrier element is _strong_ iff it is neither weak nor broken.
//!
//! The formulae for a strong and for a broken point _x_ may be written
//! as
//!
//! > <tt>IsStrong</tt>(_x_)&nbsp;&equiv;&nbsp;&forall;<sub>_y_</sub>&nbsp; (_x_ &rarr; _y_ &hArr; _y_ &larr; _x_) &nbsp;&and;&nbsp; (_x_ &larr; _y_ &hArr; _y_ &rarr; _x_),<br>
//! > <tt>IsBroken</tt>(_x_)&nbsp;&equiv;&nbsp;(&exist;<sub>_y_,_z_</sub>&nbsp; _x_ &rarr; _y_ &and; _z_ &larr; _x_ &and; _y_ &nlarr; _x_)&nbsp;
//! > &or; &nbsp;(&exist;<sub>_y_,_z_</sub>&nbsp; _x_ &larr; _y_ &and; _z_ &rarr; _x_ &and; _y_ &nrarr; _x_).
//!
//! A point is _isolated_ in a fuset _&phi;_ iff it is neither in the
//! carrier nor in the residue of _&phi;_.  We say, that any
//! non-isolated point _occurs_ in _&phi;_.
//!
//! ### Derived arming and framing relations
//!
//! Starting from an arming or framing structure of a fuset one may
//! derive several new relations.  One possibility is to lift arming
//! domain from points to sets of points (possibly empty), where the
//! lifting may be one-sided (point-by-set or set-by-point) or
//! two-sided (set-by-set).  This may be done &ldquo;by
//! necessity&rdquo; &mdash; by requiring that a property of a point
//! holds for each element of the pointset (hence, having a property
//! is necessary to be included).  The other option is lifting
//! &ldquo;by sufficiency&rdquo; &mdash; by requiring that every point
//! with a property is in the pointset (hence, having a property is
//! sufficient to be a member).  By default, arming is lifted by
//! necessity, but framing &mdash; by sufficiency.
//!
//! For example, a subset ***Y*** of ***X*** is said to be
//! _fork-armed_ (_join-armed_) by a point _x_ in a fuset _&phi;_ over
//! ***X*** iff every element of ***Y*** is fork-armed (join-armed) by
//! _x_ in _&phi;_.  Conversely, a point _x_ is _fork-armed_
//! (_join-armed_) by a subset ***Y*** of ***X*** in a fuset _&phi;_
//! over ***X*** iff _x_ is fork-armed (join-armed) in _&phi;_ by
//! every element of ***Y***.
//!
//! By contrast, a point _x_ is _fork-framed_ (_join-framed_) by a
//! subset ***Y*** of ***X*** in a fuset _&phi;_ over ***X*** iff
//! ***Y*** includes the entire pot of every _x_-tipped fork (join) in
//! _&phi;_.  Conversely, a pointset ***Y*** is _fork-framed_
//! (_join-framed_) by a point _x_ in a fuset _&phi;_ iff any point
//! that is fork-framed (join-framed) by _x_ is also a member of
//! ***Y***.
//!
//! The notion of set-by-set arming relation may take several forms.
//! When we say that a subset ***Y*** of ***X*** is _fork-armed_
//! (_join-armed_) by a subset ***Z*** of ***X***, we mean that each
//! element of ***Y*** is fork-armed (join-armed) by ***Z*** &mdash;
//! or, equivalently, that ***Y*** is fork-armed (join-armed) by every
//! element of ***Z***.  On the other hand, by saying that ***Y*** is
//! _framed_ (or _fork-framed_, or _join-framed_) by ***Z*** we mean
//! that if ***Y*** contains a tip of a wedge (or fork, or join) with
//! an arm in ***Z*** then ***Z*** contains the entire pot of that
//! wedge.
//!
//! There are also weaker variants of set-by-set arming (weaker in the
//! sense of inclusion order of relations).  For instance, a subset
//! ***Y*** of ***X*** is _pre-armed_ (_post-armed_) by a subset
//! ***Z*** of ***X*** iff each element of ***Y*** is fork-armed
//! (join-armed) by some element of ***Z*** &mdash; in other words,
//! ***Y*** is a subset of pre- or post-set containing no ***Z***-free
//! tips.  Finally, a subset ***Y*** of ***X*** is _under-armed_
//! (_over-armed_) by a subset ***Z*** of ***X*** iff each element of
//! ***Z*** is fork-arming (join-arming) some element of ***Y***
//! &mdash; ***Z*** is a subset of under- or over-set containing no
//! ***Y***-free arms.
//!
//! Likewise, set-by-set framing has stronger variants: ***Y*** is
//! _under-framed_ (_over-framed_) by ***Z*** iff ***Z*** contains the
//! entire pot of every fork (join) tipped in ***Y***.
//!
//! FIXME arming and framing of/by a fuset.
//!
//! Another derivation is that of the transitive closure.  Given a
//! fuset _&phi;_, a point _y_ is _fork-reachable_ (_join-reachable_)
//! from a point _x_ in _&phi;_ iff _y_ is fork-arming (join-arming)
//! the point _x_ or some other point which is fork-reachable
//! (join-reachable) from _x_ in _&phi;_.  Symbolically,
//!
//! > _x_ &Rarr;<sub>_&phi;_</sub> _y_ holds if _y_ is fork-reachable from _x_ in _&phi;_, and<br>
//! > _x_ &Larr;<sub>_&phi;_</sub> _y_ holds if _y_ is join-reachable from _x_ in _&phi;_.
//!
//! Reachability my be lifted in a similar way to arming: for example,
//! a subset ***Y*** of ***X*** is _fork-reachable_ (_join-reachable_)
//! from a point _x_ in a fuset _&phi;_ over ***X*** iff every element
//! of ***Y*** is fork-reachable (join-reachable) from _x_ in _&phi;_.
//! Other variants of set-by-point, point-by-set and set-by-set
//! reachability relation may be derived in the same way as the
//! corresponding set-based arming relations.
//!
//! ### Classification of fusets
//!
//! A non-empty fuset may be _bipolar_, if its pre-set and post-set
//! are both non-empty, or _unipolar_ otherwise.  Note, that all
//! proper fusets are bipolar.  A fuset is _thin_ if its pre-set and
//! post-set are disjoint (its interior is empty) or it is _thick_
//! otherwise.
//!
//! A _dipole_ is a minimal bipolar thin fuset, not necessarily proper
//! (it may contain loops).  Note, that any dipole contains exactly
//! one fork, exactly one join, and its carrier has exactly two
//! elements.  A dipole that is proper and has intersecting carrier
//! and range is called a _semifusor_.  Moreover, if carrier is a
//! subset of range, then a semifusor is called a _fusor_.
//!
//! Given a fuset _&phi;_, two wedges _w<sub>1</sub>_ and
//! _w<sub>2</sub>_ are _connected through &phi;_ iff they are the two
//! elements of a fusor or there is a wedge in _&phi;_ which is
//! connected through _&phi;_ to _w<sub>1</sub>_ and to
//! _w<sub>2</sub>_.  If such _w<sub>1</sub>_ and _w<sub>2</sub>_ are
//! also elements of _&phi;_, then they are said to be _connected in
//! &phi;_.
//!
//! A fuset _&phi;_ is _connected_ iff all wedges of _&phi;_ are
//! pairwise connected in _&phi;_.  A _fusible_ is any fuset that is
//! connected and singular.
//!
//! A fuset containing only single-arm wedges is called _primitive_.
//!
//! A _star_ is a thin fuset with a singleton carrier.  All stars are
//! unipolar and thus improper (note, that singleton-carrier fusets
//! are either proper or thin, but not both).  If a star _&alpha;_ is
//! a subset of a fuset _&phi;_, then we say that it is a _star of_
//! _x_ _in_ _&phi;_, where _x_ is the only carrier point.  A fuset
//! _&phi;_ is _singular_ iff all stars in _&phi;_ are singletons.
//! The _star partition_ of a fuset _&phi;_ is the set of all maximal
//! stars in _&phi;_.
//!
//! Given a fuset _&phi;_ and a point _x_, the maximal fork-containing
//! star of _x_ in _&phi;_ is called the _lower star_ of _x_ in
//! _&phi;_ and is denoted by _x_<sub>&star;</sub>(_&phi;_), or simply
//! by _x_<sub>&star;</sub>.  Similarly, _x_<sup>&star;</sup>(_&phi;_)
//! denotes the _upper star_ of _x_ in _&phi;_ &mdash; the maximal
//! join-containing star of _x_ in _&phi;_.  Of course, the sets
//! <tt>Pre</tt>(_x_<sup>&star;</sup>),
//! <tt>Post</tt>(_x_<sub>&star;</sub>),
//! <tt>Under</tt>(_x_<sup>&star;</sup>), and
//! <tt>Over</tt>(_x_<sub>&star;</sub>) are all empty for any point
//! _x_ in any fuset.
//!
//! A _gazer_ (&ldquo;co-star&rdquo;) is a unipolar fuset with
//! non-empty frame intersection.  The _pre-gazer_ (_post-gazer_) of a
//! point _x_ in a fuset _&phi;_ is the maximal set of forks (joins)
//! of _&phi;_ armed by _x_.  Symbolically,
//!
//! > <sup>&oast;</tt></sup>_x_(_&phi;_) denotes the pre-gazer of _x_ in _&phi;_, and<br>
//! > _x_<sup>&oast;</sup>(_&phi;_) denotes the post-gazer of _x_ in _&phi;_.
//!
//! A fuset _&phi;_ is _symmetric_ iff it is isomorphic to its flip:
//! _&phi;_ &cong; <tt>Flip</tt>(_&phi;_).
//!
//! A _coherent_ fuset is such a fuset _&phi;_ that the necessary and
//! sufficient condition for any point _x_ to be fork-armed by any
//! point _y_ in _&phi;_ is join-arming of _y_ by _x_ in _&phi;_.
//! Equivalently, the only points occuring in a coherent fuset are
//! strong sources, strong sinks, and strong internals, i.e. a
//! coherent fuset generates a domain partition containing no more
//! than the four classes listed in bold face in the main diagonal of
//! the domain partition table.
//!
//! An _arrow_ is a coherent dipole or, equivalently &mdash; a
//! primitive fusor.
//!
//! A fuset is _tight_ iff any fork's tip is in every join's pot and
//! any join's tip is in every fork's pot.  A tight dipole is called a
//! _quasifusor_ (note, that all proper quasifusors are fusors, hence
//! all fusors are tight).
//!
//! * **Example 5.** The fuset
//!   <tt>**{**(_a_,{_x_,_y_}),({_a_},_x_),({_a_},_y_)**}**</tt> is
//!   coherent and tight.  It is the union of two (incoherent) fusors:
//!   <tt>**{**(_a_,{_x_,_y_}),({_a_},_x_)**}**</tt> and
//!   <tt>**{**(_a_,{_x_,_y_}),({_a_},_y_)**}**</tt>.
//!
//! * **Example 6.** The fuset
//!   <tt>**{**(_a_,{_x_}),(_b_,{_x_,_y_}),(_c_,{_y_}),({_a_,_b_},_x_),({_b_,_c_},_y_)**}**</tt>
//!   is coherent but not tight.

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

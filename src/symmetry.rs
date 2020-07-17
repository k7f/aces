use std::{
    collections::{HashSet, BTreeSet},
    cmp::Ordering,
};
use crate::{AcesError, AcesErrorKind};

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct Permutation {
    word: Vec<usize>,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Cycles {
    factors: Vec<Vec<usize>>,
    size:    usize,
}

impl Permutation {
    pub fn new<I>(it: I) -> Result<Self, AcesError>
    where
        I: IntoIterator<Item = usize>,
    {
        let mut word = Vec::new();
        let mut values = HashSet::new();
        let mut max_value = 0;

        for v in it.into_iter() {
            if values.insert(v) {
                if max_value < v {
                    max_value = v;
                }

                word.push(v);
            } else {
                return Err(AcesErrorKind::NotAPermutationDueToRepetition(v).into())
            }
        }

        match word.len() {
            n if n == 0 || n == max_value + 1 => Ok(Permutation { word }),
            n if n <= max_value => {
                Err(AcesErrorKind::NotAPermutationDueToOverflow(max_value, n).into())
            }
            _ => unreachable!(),
        }
    }

    /// This should be interpreted as the (only) permutation of the
    /// empty set.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.word.is_empty()
    }

    #[inline]
    pub fn size(&self) -> usize {
        self.word.len()
    }

    pub fn is_identity(&self) -> bool {
        self.word.iter().enumerate().all(|(p, v)| *v == p)
    }

    #[inline]
    pub fn transpose(&mut self, pos1: usize, pos2: usize) {
        debug_assert!(pos1 < self.word.len());
        debug_assert!(pos2 < self.word.len());

        self.word.swap(pos1, pos2);
    }

    pub fn invert(&mut self) {
        for (pos, image) in self.word.clone().into_iter().enumerate() {
            self.word[image] = pos;
        }
    }

    pub fn compose_with(&mut self, other: &Self) {
        if self.word.len() < other.word.len() {
            self.word.extend(self.word.len()..other.word.len());
        }

        for image in self.word.iter_mut() {
            if let Some(new_image) = other.word.get(*image) {
                *image = *new_image;
            }
        }
    }

    /// Decompose this permutation into cycles.
    ///
    /// The cycles are stored according to the standard representation
    /// (they are canonically ordered).
    pub fn as_cycles(&self) -> Cycles {
        let mut factors = BTreeSet::new();
        let size = self.word.len();
        let mut visited = vec![false; size];

        for pos in 0..size {
            if !visited[pos] {
                let mut image = self.word[pos];

                if image > pos {
                    let mut cycle = vec![pos];
                    let mut ndx = 0;
                    let mut max_ndx = 0;

                    visited[image] = true;

                    loop {
                        cycle.push(image);

                        ndx += 1;
                        if image > cycle[max_ndx] {
                            max_ndx = ndx;
                        }

                        image = self.word[image];

                        match image.cmp(&pos) {
                            Ordering::Greater => visited[image] = true,
                            Ordering::Equal => break,
                            Ordering::Less => {}
                        }
                    }

                    if max_ndx > 0 {
                        cycle.rotate_left(max_ndx);
                    }

                    factors.insert(cycle);
                }
            }
        }

        Cycles { factors: factors.into_iter().collect(), size: self.word.len() }
    }

    /// Performs a single step of Heap's (minimal change) iteration.
    ///
    /// Returns `true` after applying a single transposition, or
    /// `false` if none of `stack` elements is less than its index
    /// (end of iteration).  In order to traverse _S<sub>n</sub>_,
    /// start from any _n_-element `Permutation`, initialize `stack`
    /// with _n_ zeros and repeatedly call `heap_step()` until the
    /// returned value is `false`.
    pub fn heap_step(&mut self, stack: &mut [usize]) -> bool {
        debug_assert!(stack.len() >= self.word.len());

        let mut pos = 0;

        while pos < self.word.len() {
            let s = stack[pos];

            if s < pos {
                if pos % 2 == 0 {
                    self.word.swap(0, pos);
                } else {
                    self.word.swap(s, pos);
                }

                stack[pos] += 1;

                return true
            } else {
                stack[pos] = 0;
                pos += 1;
            }
        }

        false
    }
}

impl Cycles {
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.factors.is_empty()
    }

    #[inline]
    pub fn size(&self) -> usize {
        self.size
    }

    // FIXME type
}

impl From<Cycles> for Permutation {
    fn from(cycles: Cycles) -> Self {
        let mut result = Permutation::new(0..cycles.size()).unwrap();

        for cycle in cycles.factors {
            if let Some(mut pos) = cycle.last().copied() {
                for image in cycle {
                    result.word[pos] = image;
                    pos = image;
                }
            }
        }

        result
    }
}

#[cfg(test)]
mod tests {
    use std::{iter::FromIterator, cmp::Ordering};
    use super::*;

    #[test]
    fn test_order() {
        let mut perm1 = Permutation::new(0..9).unwrap();
        let mut perm2 = perm1.clone();
        perm2.transpose(5, 6);
        assert_eq!(perm1.cmp(&perm2), Ordering::Less);
        perm1.transpose(5, 8);
        assert_eq!(perm1.cmp(&perm2), Ordering::Greater);
        perm1.transpose(5, 6);
        perm2.transpose(6, 8);
        assert_eq!(perm1.cmp(&perm2), Ordering::Equal);
    }

    #[test]
    fn test_identity() {
        let perm = Permutation::new(0..9).unwrap();
        assert!(perm.is_identity());
    }

    #[test]
    fn test_inverse() {
        let perm1 = Permutation::new(0..9).unwrap();
        let mut perm2 = perm1.clone();
        perm2.invert();
        assert_eq!(perm1, perm2);

        let mut perm1 = Permutation::new(vec![3, 0, 1, 2, 4]).unwrap();
        let mut perm2 = perm1.clone();
        perm2.invert();
        perm1.compose_with(&perm2);
        assert!(perm1.is_identity());
    }

    #[test]
    fn test_cycles() {
        let perm = Permutation::new(0..9).unwrap();
        let cycles = perm.as_cycles();
        assert!(cycles.is_empty());
        assert_eq!(perm, cycles.into());

        let mut perm = Permutation::new(vec![1, 0, 3, 4, 2]).unwrap();
        let cycles = perm.as_cycles();
        assert_eq!(cycles, Cycles { factors: vec![vec![1, 0], vec![4, 2, 3]], size: perm.size() });
        assert_eq!(perm, cycles.into());

        perm.transpose(1, 2);
        let cycles = perm.as_cycles();
        assert_eq!(cycles, Cycles { factors: vec![vec![4, 2, 0, 1, 3]], size: perm.size() });
        assert_eq!(perm, cycles.into());

        perm.transpose(0, 3);
        let cycles = perm.as_cycles();
        assert_eq!(cycles, Cycles { factors: vec![vec![3, 1], vec![4, 2, 0]], size: perm.size() });
        assert_eq!(perm, cycles.into());
    }

    #[test]
    fn test_heap() {
        let mut perm = Permutation::new(0..9).unwrap();
        let mut stack = Vec::from_iter(0..9);
        assert!(!perm.heap_step(stack.as_mut_slice()));

        let mut stack = vec![0; 9];
        assert!(perm.heap_step(stack.as_mut_slice()));
        assert_eq!(perm, Permutation { word: vec![1, 0, 2, 3, 4, 5, 6, 7, 8] });

        let mut perm = Permutation::new(vec![1, 0, 3, 4, 2]).unwrap();
        let mut stack = vec![0; 5];
        perm.heap_step(stack.as_mut_slice());
        perm.heap_step(stack.as_mut_slice());
        assert_eq!(perm, Permutation { word: vec![3, 1, 0, 4, 2] });
    }
}

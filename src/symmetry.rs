use std::{
    collections::{HashSet, BTreeSet},
    cmp::Ordering,
};
use rand::{Rng, seq::SliceRandom};
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

    pub fn shuffle<R: Rng + ?Sized>(&mut self, rng: &mut R) {
        self.word.shuffle(rng);
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

    /// Computes the lexicographic rank of this permutation.
    ///
    /// Note, that the result is correct only if `self.size()` &leq;
    /// 20 (due to integer overflow).
    pub fn rank(&self) -> u64 {
        debug_assert!(self.word.len() <= 20);

        let mut result = 0_u64;
        let mut table = vec![0; self.word.len() + 1];
        let size = self.word.len() as i32;
        let mut pos = size as u64;

        for &image in self.word.iter() {
            let mut ctr = image;

            let mut node = image as i32;
            while node > 0 {
                ctr -= table[node as usize];
                node &= node - 1;
            }

            result = pos * result + ctr as u64;
            pos -= 1;

            node = image as i32 + 1;
            while node < size {
                table[node as usize] += 1;
                node += node & -node;
            }
        }

        result
    }

    /// Creates a new permutation based on `size` and (lexicographic)
    /// `rank`.
    ///
    /// Note, that the result is correct only for `size` &leq; 20 (due
    /// to integer overflow).
    pub fn from_rank(mut rank: u64, size: usize) -> Self {
        debug_assert!(size <= 20);

        let mut word = Vec::with_capacity(size);
        let mut digits = vec![0; size];

        for i in 2..=size {
            digits[size - i] = rank % i as u64;
            rank /= i as u64;
        }

        let mut table: Vec<_> = (0..=size as i32).map(|node| node & -node).collect();

        for mut digit in digits {
            let mut image = size;

            for shift in &[1, 2, 4, 8, 16] {
                image |= image >> shift;
            }
            image += 1;

            let mut node = image;
            while node > 0 {
                if image <= size && table[image] as u64 <= digit {
                    digit -= table[image] as u64;
                } else {
                    image ^= node;
                }

                node >>= 1;
                image |= node;
            }

            node = image + 1;
            while node < size {
                table[node] -= 1;
                node += (node as i32 & -(node as i32)) as usize;
            }

            word.push(image);
        }

        Permutation { word }
    }

    // pub fn lehmer(&self)
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

    /// Computes the cycle type (conjugacy class signature) of this
    /// permutation.
    ///
    /// The size of the returned `Vec` is always equal to the size of
    /// the permutation.  The element at index _i_ is the number of
    /// cycles of length _i_ + 1.
    pub fn cycle_type(&self) -> Vec<usize> {
        let mut result = vec![0; self.size];
        let mut sum = 0;

        for len in self.factors.iter().map(|c| c.len()) {
            result[len] += 1;
            sum += len;
        }

        result[0] = self.size - sum;

        result
    }
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
    fn test_rank() {
        let mut rng = rand::thread_rng();
        for size in 0..20 {
            let mut perm = Permutation::new(0..size).unwrap();
            assert_eq!(perm.rank(), 0);
            assert_eq!(perm, Permutation::from_rank(0, size));
            for _ in 0..10 {
                perm.shuffle(&mut rng);
                assert_eq!(perm, Permutation::from_rank(perm.rank(), perm.size()));
            }
        }
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
    fn test_cycle_type() {
        let mut perm = Permutation::new(0..9).unwrap();
        let cycles = perm.as_cycles();
        let ctype = cycles.cycle_type();
        assert_eq!(ctype, vec![9, 0, 0, 0, 0, 0, 0, 0, 0]);

        let mut stack = vec![0; 9];
        for _ in 0..123456 {
            perm.heap_step(stack.as_mut_slice());
        }
        let cycles = perm.as_cycles();
        let ctype = cycles.cycle_type();
        assert_eq!(ctype, vec![0, 0, 0, 3, 0, 0, 0, 0, 0]);
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

    #[test]
    fn test_heap_s9() {
        let mut perm = Permutation::new(0..9).unwrap();
        let mut stack = vec![0; 9];
        let mut count = 1_u64;

        while perm.heap_step(stack.as_mut_slice()) {
            count += 1;
        }

        assert_eq!(count, 362880);
        assert_eq!(perm, Permutation { word: vec![8, 1, 2, 3, 4, 5, 6, 7, 0] });
    }
}

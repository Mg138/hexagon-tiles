use std::cmp::max;
use std::ops::{Add, Div, Mul, Neg, Sub};

use num::{Float, NumCast, PrimInt, Signed};

use crate::fractional::frac_hex;
use crate::traits::{HexDirection, HexMath, HexRotate, HexRound};

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, Default)]
pub struct PackedHex<I> {
    q: I,
    r: I,
}

pub const fn packed_hex<I: PrimInt + Neg<Output = I>>(q: I, r: I) -> PackedHex<I> {
    PackedHex::new(q, r)
}

impl<I: PrimInt + Neg<Output = I>> From<Hex<I>> for PackedHex<I> {
    fn from(value: Hex<I>) -> Self {
        Self::new(value.q, value.r)
    }
}

impl<I: PrimInt + Neg<Output = I>> From<PackedHex<I>> for Hex<I> {
    fn from(value: PackedHex<I>) -> Self {
        Self::new(value.q, value.r)
    }
}

impl From<i32> for PackedHex<i16> {
    fn from(value: i32) -> Self {
        let q = (value >> 16) as i16;
        let r = (value & 0xFFFF) as i16;

        Self::new(q, r)
    }
}

impl From<PackedHex<i16>> for i32 {
    fn from(value: PackedHex<i16>) -> Self {
        (value.q as i32) << 16 | (value.r as i32)
    }
}

impl From<i64> for PackedHex<i32> {
    fn from(value: i64) -> Self {
        let q = (value >> 32) as i32;
        let r = (value & 0xFFFFFFFF) as i32;

        Self::new(q, r)
    }
}

impl From<PackedHex<i32>> for i64 {
    fn from(value: PackedHex<i32>) -> Self {
        (value.q as i64) << 32 | (value.r as i64)
    }
}

impl<I: PrimInt + Neg<Output = I>> PackedHex<I> {
    pub const fn new(q: I, r: I) -> Self {
        Self { q, r }
    }

    pub fn q(&self) -> I {
        self.q
    }

    pub fn r(&self) -> I {
        self.r
    }

    pub fn s(&self) -> I {
        -self.q - self.r
    }
}

impl<I: PrimInt> Add for PackedHex<I> {
    type Output = Self;

    fn add(self, other: Self) -> Self::Output {
        let q = self.q + other.q;
        let r = self.r + other.r;

        Self { q, r }
    }
}

impl<I: PrimInt + Signed> Sub for PackedHex<I> {
    type Output = Self;

    fn sub(self, other: Self) -> Self::Output {
        let q = self.q - other.q;
        let r = self.r - other.r;

        Self { q, r }
    }
}

impl<I: PrimInt> Mul<I> for PackedHex<I> {
    type Output = Self;

    fn mul(self, k: I) -> Self::Output {
        let q = self.q * k;
        let r = self.r * k;

        Self { q, r }
    }
}

impl<I: PrimInt> Div<I> for PackedHex<I> {
    type Output = Self;

    fn div(self, k: I) -> Self::Output {
        let q = self.q / k;
        let r = self.r / k;

        Self { q, r }
    }
}

impl<I: PrimInt + Neg<Output = I>> Neg for PackedHex<I> {
    type Output = Self;

    fn neg(self) -> Self::Output {
        let q = -self.q;
        let r = -self.r;

        Self { q, r }
    }
}

impl HexDirection for PackedHex<i8> {
    #[rustfmt::skip]
    const NEIGHBORS: [Self; 6] = [
        packed_hex( 1,  0),
        packed_hex( 1, -1),
        packed_hex( 0, -1),
        packed_hex(-1,  0),
        packed_hex(-1,  1),
        packed_hex( 0,  1),
    ];

    #[rustfmt::skip]
    const DIAGONALS: [Self; 6] = [
        packed_hex( 2, -1),
        packed_hex( 1, -2),
        packed_hex(-1, -1),
        packed_hex(-2,  1),
        packed_hex(-1,  2),
        packed_hex( 1,  1),
    ];
}

impl HexDirection for PackedHex<i16> {
    #[rustfmt::skip]
    const NEIGHBORS: [Self; 6] = [
        packed_hex( 1,  0),
        packed_hex( 1, -1),
        packed_hex( 0, -1),
        packed_hex(-1,  0),
        packed_hex(-1,  1),
        packed_hex( 0,  1),
    ];

    #[rustfmt::skip]
    const DIAGONALS: [Self; 6] = [
        packed_hex( 2, -1),
        packed_hex( 1, -2),
        packed_hex(-1, -1),
        packed_hex(-2,  1),
        packed_hex(-1,  2),
        packed_hex( 1,  1),
    ];
}

impl HexDirection for PackedHex<i32> {
    #[rustfmt::skip]
    const NEIGHBORS: [Self; 6] = [
        packed_hex( 1,  0),
        packed_hex( 1, -1),
        packed_hex( 0, -1),
        packed_hex(-1,  0),
        packed_hex(-1,  1),
        packed_hex( 0,  1),
    ];

    #[rustfmt::skip]
    const DIAGONALS: [Self; 6] = [
        packed_hex( 2, -1),
        packed_hex( 1, -2),
        packed_hex(-1, -1),
        packed_hex(-2,  1),
        packed_hex(-1,  2),
        packed_hex( 1,  1),
    ];
}

impl HexDirection for PackedHex<i64> {
    #[rustfmt::skip]
    const NEIGHBORS: [Self; 6] = [
        packed_hex( 1,  0),
        packed_hex( 1, -1),
        packed_hex( 0, -1),
        packed_hex(-1,  0),
        packed_hex(-1,  1),
        packed_hex( 0,  1),
    ];

    #[rustfmt::skip]
    const DIAGONALS: [Self; 6] = [
        packed_hex( 2, -1),
        packed_hex( 1, -2),
        packed_hex(-1, -1),
        packed_hex(-2,  1),
        packed_hex(-1,  2),
        packed_hex( 1,  1),
    ];
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, Default)]
pub struct Hex<I> {
    q: I,
    r: I,
    s: I,
}

pub const fn hex<I>(q: I, r: I, s: I) -> Hex<I> {
    Hex { q, r, s }
}

impl<I: PrimInt + Neg<Output = I>> Hex<I> {
    pub fn new(q: I, r: I) -> Self {
        let s = -q - r;

        Self { q, r, s }
    }

    pub fn q(&self) -> I {
        self.q
    }

    pub fn r(&self) -> I {
        self.r
    }

    pub fn s(&self) -> I {
        self.s
    }
}

impl<I: PrimInt> Add for Hex<I> {
    type Output = Self;

    fn add(self, other: Self) -> Self::Output {
        let q = self.q + other.q;
        let r = self.r + other.r;
        let s = self.s + other.s;

        Self { q, r, s }
    }
}

impl<I: PrimInt + Signed> Sub for Hex<I> {
    type Output = Self;

    fn sub(self, other: Self) -> Self::Output {
        let q = self.q - other.q;
        let r = self.r - other.r;
        let s = self.s - other.s;

        Self { q, r, s }
    }
}

impl<I: PrimInt> Mul<I> for Hex<I> {
    type Output = Self;

    fn mul(self, k: I) -> Self::Output {
        let q = self.q * k;
        let r = self.r * k;
        let s = self.s * k;

        Self { q, r, s }
    }
}

impl<I: PrimInt> Div<I> for Hex<I> {
    type Output = Self;

    fn div(self, k: I) -> Self::Output {
        let q = self.q / k;
        let r = self.r / k;
        let s = self.s / k;

        Self { q, r, s }
    }
}

impl<I: PrimInt + Neg<Output = I>> Neg for Hex<I> {
    type Output = Self;

    fn neg(self) -> Self::Output {
        let q = -self.q;
        let r = -self.r;
        let s = -self.s;

        Self { q, r, s }
    }
}

impl HexDirection for Hex<i8> {
    #[rustfmt::skip]
    const NEIGHBORS: [Self; 6] = [
        hex( 1,  0, -1),
        hex( 1, -1,  0),
        hex( 0, -1,  1),
        hex(-1,  0,  1),
        hex(-1,  1,  0),
        hex( 0,  1, -1),
    ];

    #[rustfmt::skip]
    const DIAGONALS: [Self; 6] = [
        hex( 2, -1, -1),
        hex( 1, -2,  1),
        hex(-1, -1,  2),
        hex(-2,  1,  1),
        hex(-1,  2, -1),
        hex( 1,  1, -2),
    ];
}

impl HexDirection for Hex<i16> {
    #[rustfmt::skip]
    const NEIGHBORS: [Self; 6] = [
        hex( 1,  0, -1),
        hex( 1, -1,  0),
        hex( 0, -1,  1),
        hex(-1,  0,  1),
        hex(-1,  1,  0),
        hex( 0,  1, -1),
    ];

    #[rustfmt::skip]
    const DIAGONALS: [Self; 6] = [
        hex( 2, -1, -1),
        hex( 1, -2,  1),
        hex(-1, -1,  2),
        hex(-2,  1,  1),
        hex(-1,  2, -1),
        hex( 1,  1, -2),
    ];
}

impl HexDirection for Hex<i32> {
    #[rustfmt::skip]
    const NEIGHBORS: [Self; 6] = [
        hex( 1,  0, -1),
        hex( 1, -1,  0),
        hex( 0, -1,  1),
        hex(-1,  0,  1),
        hex(-1,  1,  0),
        hex( 0,  1, -1),
    ];

    #[rustfmt::skip]
    const DIAGONALS: [Self; 6] = [
        hex( 2, -1, -1),
        hex( 1, -2,  1),
        hex(-1, -1,  2),
        hex(-2,  1,  1),
        hex(-1,  2, -1),
        hex( 1,  1, -2),
    ];
}

impl HexDirection for Hex<i64> {
    #[rustfmt::skip]
    const NEIGHBORS: [Self; 6] = [
        hex( 1,  0, -1),
        hex( 1, -1,  0),
        hex( 0, -1,  1),
        hex(-1,  0,  1),
        hex(-1,  1,  0),
        hex( 0,  1, -1),
    ];

    #[rustfmt::skip]
    const DIAGONALS: [Self; 6] = [
        hex( 2, -1, -1),
        hex( 1, -2,  1),
        hex(-1, -1,  2),
        hex(-2,  1,  1),
        hex(-1,  2, -1),
        hex( 1,  1, -2),
    ];
}

impl<I: PrimInt + Neg<Output = I>> HexRotate for Hex<I> {
    fn rotate_left(self) -> Self {
        Self {
            q: -self.s,
            r: -self.q,
            s: -self.r,
        }
    }

    fn rotate_right(self) -> Self {
        Self {
            q: -self.r,
            r: -self.s,
            s: -self.q,
        }
    }
}

impl HexMath<i8> for Hex<i8> {
    fn length(self) -> i8 {
        (self.q.abs() + self.r.abs() + self.s.abs()) / 2
    }

    /**
     * F is the intermediate type.
     */
    fn line<F: Float>(self, to: Self) -> Vec<Self> {
        let n = self.distance(to);

        let small_one: F = NumCast::from(1e-06).unwrap();
        let small_two: F = NumCast::from(2e-06).unwrap();

        let a_nudge = {
            let q = <F as NumCast>::from(self.q).unwrap() + small_one;
            let r = <F as NumCast>::from(self.r).unwrap() + small_one;
            let s = <F as NumCast>::from(self.s).unwrap() - small_two;

            frac_hex(q, r, s)
        };

        let b_nudge = {
            let q = <F as NumCast>::from(to.q).unwrap() + small_one;
            let r = <F as NumCast>::from(to.r).unwrap() + small_one;
            let s = <F as NumCast>::from(to.s).unwrap() - small_two;

            frac_hex(q, r, s)
        };

        let step = F::one() / NumCast::from(max(n, 1)).unwrap();

        (0..=n)
            .map(|i| {
                let i: F = NumCast::from(i).unwrap();

                a_nudge.lerp(b_nudge, step * i).round()
            })
            .collect()
    }
}

impl HexMath<i16> for Hex<i16> {
    fn length(self) -> i16 {
        (self.q.abs() + self.r.abs() + self.s.abs()) / 2
    }

    /**
     * F is the intermediate type.
     */
    fn line<F: Float>(self, to: Self) -> Vec<Self> {
        let n = self.distance(to);

        let small_one: F = NumCast::from(1e-06).unwrap();
        let small_two: F = NumCast::from(2e-06).unwrap();

        let a_nudge = {
            let q = <F as NumCast>::from(self.q).unwrap() + small_one;
            let r = <F as NumCast>::from(self.r).unwrap() + small_one;
            let s = <F as NumCast>::from(self.s).unwrap() - small_two;

            frac_hex(q, r, s)
        };

        let b_nudge = {
            let q = <F as NumCast>::from(to.q).unwrap() + small_one;
            let r = <F as NumCast>::from(to.r).unwrap() + small_one;
            let s = <F as NumCast>::from(to.s).unwrap() - small_two;

            frac_hex(q, r, s)
        };

        let step = F::one() / NumCast::from(max(n, 1)).unwrap();

        (0..=n)
            .map(|i| {
                let i: F = NumCast::from(i).unwrap();

                a_nudge.lerp(b_nudge, step * i).round()
            })
            .collect()
    }
}

impl HexMath<i32> for Hex<i32> {
    fn length(self) -> i32 {
        (self.q.abs() + self.r.abs() + self.s.abs()) / 2
    }

    /**
     * F is the intermediate type.
     */
    fn line<F: Float>(self, to: Self) -> Vec<Self> {
        let n = self.distance(to);

        let small_one: F = NumCast::from(1e-06).unwrap();
        let small_two: F = NumCast::from(2e-06).unwrap();

        let a_nudge = {
            let q = <F as NumCast>::from(self.q).unwrap() + small_one;
            let r = <F as NumCast>::from(self.r).unwrap() + small_one;
            let s = <F as NumCast>::from(self.s).unwrap() - small_two;

            frac_hex(q, r, s)
        };

        let b_nudge = {
            let q = <F as NumCast>::from(to.q).unwrap() + small_one;
            let r = <F as NumCast>::from(to.r).unwrap() + small_one;
            let s = <F as NumCast>::from(to.s).unwrap() - small_two;

            frac_hex(q, r, s)
        };

        let step = F::one() / NumCast::from(max(n, 1)).unwrap();

        (0..=n)
            .map(|i| {
                let i: F = NumCast::from(i).unwrap();

                a_nudge.lerp(b_nudge, step * i).round()
            })
            .collect()
    }
}

impl HexMath<i64> for Hex<i64> {
    fn length(self) -> i64 {
        (self.q.abs() + self.r.abs() + self.s.abs()) / 2
    }

    /**
     * F is the intermediate type.
     */
    fn line<F: Float>(self, to: Self) -> Vec<Self> {
        let n = self.distance(to);

        let small_one: F = NumCast::from(1e-06).unwrap();
        let small_two: F = NumCast::from(2e-06).unwrap();

        let a_nudge = {
            let q = <F as NumCast>::from(self.q).unwrap() + small_one;
            let r = <F as NumCast>::from(self.r).unwrap() + small_one;
            let s = <F as NumCast>::from(self.s).unwrap() - small_two;

            frac_hex(q, r, s)
        };

        let b_nudge = {
            let q = <F as NumCast>::from(to.q).unwrap() + small_one;
            let r = <F as NumCast>::from(to.r).unwrap() + small_one;
            let s = <F as NumCast>::from(to.s).unwrap() - small_two;

            frac_hex(q, r, s)
        };

        let step = F::one() / NumCast::from(max(n, 1)).unwrap();

        (0..=n)
            .map(|i| {
                let i: F = NumCast::from(i).unwrap();

                a_nudge.lerp(b_nudge, step * i).round()
            })
            .collect()
    }
}

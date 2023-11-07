use crate::{
    constants::{MAX_PRECISION, MAX_STR_BUFFER_SIZE, MIGHT_OVERFLOW_U64_BYTES, OVERFLOW_U96, WILL_OVERFLOW_U64},
    error::{tail_error, Error},
    ops::array::{add_by_internal_flattened, add_one_internal, div_by_u32, is_all_zero, mul_by_u32},
    Decimal,
};

use arrayvec::{ArrayString, ArrayVec};

use alloc::{string::String, vec::Vec};
use core::fmt;

pub trait StrParser: Sized {
    const MAX_SCALE: u8;
    const HANDLES_SCIENTIFIC: bool = false;
    type Error;
    fn overflows_at(mantissa: u128, scale: u8) -> bool;
    fn from_scale(mantissa: u128, scale: u8, is_negative: bool) -> Result<Self, Self::Error>;
    fn from_scientific(mantissa: u128, scale: u8, is_negative: bool, exponent: i32) -> Result<Self, Self::Error> {
        let _ = (mantissa, scale, is_negative, exponent);
        unreachable!()
    }
    fn error_from_string(err: &'static str) -> Self::Error;
}

impl StrParser for Decimal {
    const MAX_SCALE: u8 = 28;
    type Error = crate::Error;

    fn overflows_at(mantissa: u128, _: u8) -> bool {
        mantissa >= OVERFLOW_U96
    }
    fn error_from_string(err: &'static str) -> Self::Error {
        err.into()
    }
    fn from_scale(mantissa: u128, scale: u8, is_negative: bool) -> Result<Self, Self::Error> {
        debug_assert_eq!(mantissa >> 96, 0);
        Ok(Decimal::from_parts(
            mantissa as u32,
            (mantissa >> 32) as u32,
            (mantissa >> 64) as u32,
            is_negative,
            scale as u32,
        ))
    }
}

// impl that doesn't allocate for serialization purposes.
pub(crate) fn to_str_internal(
    value: &Decimal,
    append_sign: bool,
    precision: Option<usize>,
) -> (ArrayString<MAX_STR_BUFFER_SIZE>, Option<usize>) {
    // Get the scale - where we need to put the decimal point
    let scale = value.scale() as usize;

    // Convert to a string and manipulate that (neg at front, inject decimal)
    let mut chars = ArrayVec::<_, MAX_STR_BUFFER_SIZE>::new();
    let mut working = value.mantissa_array3();
    while !is_all_zero(&working) {
        let remainder = div_by_u32(&mut working, 10u32);
        chars.push(char::from(b'0' + remainder as u8));
    }
    while scale > chars.len() {
        chars.push('0');
    }

    let (prec, additional) = match precision {
        Some(prec) => {
            let max: usize = MAX_PRECISION.into();
            if prec > max {
                (max, Some(prec - max))
            } else {
                (prec, None)
            }
        }
        None => (scale, None),
    };

    let len = chars.len();
    let whole_len = len - scale;
    let mut rep = ArrayString::new();
    // Append the negative sign if necessary while also keeping track of the length of an "empty" string representation
    let empty_len = if append_sign && value.is_sign_negative() {
        rep.push('-');
        1
    } else {
        0
    };
    let mut point = false;
    for i in 0..whole_len + prec {
        if i == len - scale {
            if i == 0 {
                rep.push('0');
            }
            rep.push('.');
            point = true;
        }

        if i >= len {
            rep.push('0');
        } else {
            let c = chars[len - i - 1];
            rep.push(c);
        }
    }

    // corner case for when we truncated everything in a low fractional
    if rep.len() == empty_len {
        rep.push('0');
    }

    if precision.is_none() {
        // Remove extra zeros after the decimal point
        while rep.len() > 1 && rep.bytes().last() == Some(b'0') && point {
            rep.pop();
        }

        if rep.len() > 1 && rep.bytes().last() == Some(b'.') {
            rep.pop();
        }
    }

    (rep, additional)
}

pub(crate) fn fmt_scientific_notation(
    value: &Decimal,
    exponent_symbol: &str,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    #[cfg(not(feature = "std"))]
    use alloc::string::ToString;

    // Get the scale - this is the e value. With multiples of 10 this may get bigger.
    let mut exponent = -(value.scale() as isize);

    // Convert the integral to a string
    let mut chars = Vec::new();
    let mut working = value.mantissa_array3();
    while !is_all_zero(&working) {
        let remainder = div_by_u32(&mut working, 10u32);
        chars.push(char::from(b'0' + remainder as u8));
    }

    // First of all, apply scientific notation rules. That is:
    //  1. If non-zero digit comes first, move decimal point left so that e is a positive integer
    //  2. If decimal point comes first, move decimal point right until after the first non-zero digit
    // Since decimal notation naturally lends itself this way, we just need to inject the decimal
    // point in the right place and adjust the exponent accordingly.

    let len = chars.len();
    let mut rep;
    // We either are operating with a precision specified, or on defaults. Defaults will perform "smart"
    // reduction of precision.
    if let Some(precision) = f.precision() {
        if len > 1 {
            // If we're zero precision AND it's trailing zeros then strip them
            if precision == 0 && chars.iter().take(len - 1).all(|c| *c == '0') {
                rep = chars.iter().skip(len - 1).collect::<String>();
            } else {
                // We may still be zero precision, however we aren't trailing zeros
                if precision > 0 {
                    chars.insert(len - 1, '.');
                }
                rep = chars
                    .iter()
                    .rev()
                    // Add on extra zeros according to the precision. At least one, since we added a decimal place.
                    .chain(core::iter::repeat(&'0'))
                    .take(if precision == 0 { 1 } else { 2 + precision })
                    .collect::<String>();
            }
            exponent += (len - 1) as isize;
        } else if precision > 0 {
            // We have precision that we want to add
            chars.push('.');
            rep = chars
                .iter()
                .chain(core::iter::repeat(&'0'))
                .take(2 + precision)
                .collect::<String>();
        } else {
            rep = chars.iter().collect::<String>();
        }
    } else if len > 1 {
        // If the number is just trailing zeros then we treat it like 0 precision
        if chars.iter().take(len - 1).all(|c| *c == '0') {
            rep = chars.iter().skip(len - 1).collect::<String>();
        } else {
            // Otherwise, we need to insert a decimal place and make it a scientific number
            chars.insert(len - 1, '.');
            rep = chars.iter().rev().collect::<String>();
        }
        exponent += (len - 1) as isize;
    } else {
        rep = chars.iter().collect::<String>();
    }

    rep.push_str(exponent_symbol);
    rep.push_str(&exponent.to_string());
    f.pad_integral(value.is_sign_positive(), "", &rep)
}

// dedicated implementation for the most common case.
#[inline]
pub fn parse_str_radix_10_generic<D: StrParser>(str: &str) -> Result<D, D::Error> {
    let bytes = str.as_bytes();
    parse_bytes_radix_10_generic(bytes)
}

/// SAFETY: passed bytes must be valid utf8
#[inline]
pub fn parse_bytes_radix_10_generic<D: StrParser>(bytes: &[u8]) -> Result<D, D::Error> {
    // handle the sign

    // at 18 we overflow u64
    if bytes.len() <= (D::MAX_SCALE as usize).min(MIGHT_OVERFLOW_U64_BYTES) {
        parse_str_radix_10_dispatch::<D, false>(bytes)
    } else {
        parse_str_radix_10_dispatch::<D, true>(bytes)
    }
}

#[inline]
pub fn parse_str_radix_10(str: &str) -> Result<Decimal, crate::Error> {
    let bytes = str.as_bytes();
    if bytes.len() <= (Decimal::MAX_SCALE as usize).min(MIGHT_OVERFLOW_U64_BYTES) {
        parse_str_radix_10_dispatch::<Decimal, false>(bytes)
    } else {
        parse_str_radix_10_dispatch::<Decimal, true>(bytes)
    }
}

#[inline]
fn parse_str_radix_10_dispatch<D: StrParser, const BIG: bool>(bytes: &[u8]) -> Result<D, D::Error> {
    match bytes {
        [b, rest @ ..] => byte_dispatch_u64::<D, false, false, false, BIG, true>(rest, 0, 0, *b),
        [] => tail_error("Invalid decimal: empty"),
    }
}

#[inline]
fn overflow_64(val: u64) -> bool {
    val >= WILL_OVERFLOW_U64
}

#[inline]
fn dispatch_next<D: StrParser, const POINT: bool, const NEG: bool, const HAS: bool, const BIG: bool>(
    bytes: &[u8],
    data64: u64,
    scale: u8,
) -> Result<D, D::Error> {
    if let Some((next, bytes)) = bytes.split_first() {
        byte_dispatch_u64::<D, POINT, NEG, HAS, BIG, false>(bytes, data64, scale, *next)
    } else {
        handle_data::<D, NEG, HAS>(data64 as u128, scale)
    }
}

#[inline(never)]
fn non_digit_dispatch_u64<
    D: StrParser,
    const POINT: bool,
    const NEG: bool,
    const HAS: bool,
    const BIG: bool,
    const FIRST: bool,
>(
    bytes: &[u8],
    data64: u64,
    scale: u8,
    b: u8,
) -> Result<D, D::Error> {
    match b {
        b'-' if FIRST && !HAS => dispatch_next::<D, false, true, false, BIG>(bytes, data64, scale),
        b'+' if FIRST && !HAS => dispatch_next::<D, false, false, false, BIG>(bytes, data64, scale),
        b'_' if HAS => handle_separator::<D, POINT, NEG, BIG>(bytes, data64, scale),
        b'e' | b'E' if HAS && D::HANDLES_SCIENTIFIC => handle_scientific::<D, NEG>(bytes, data64 as u128, scale),
        b => tail_invalid_digit(b),
    }
}

#[inline]
fn byte_dispatch_u64<
    D: StrParser,
    const POINT: bool,
    const NEG: bool,
    const HAS: bool,
    const BIG: bool,
    const FIRST: bool,
>(
    bytes: &[u8],
    data64: u64,
    scale: u8,
    b: u8,
) -> Result<D, D::Error> {
    match b {
        b'0'..=b'9' => handle_digit_64::<D, POINT, NEG, BIG>(bytes, data64, scale, b - b'0'),
        b'.' if !POINT => handle_point::<D, NEG, HAS, BIG>(bytes, data64, scale),
        b => non_digit_dispatch_u64::<D, POINT, NEG, HAS, BIG, FIRST>(bytes, data64, scale, b),
    }
}

#[inline(never)]
fn handle_digit_64<D: StrParser, const POINT: bool, const NEG: bool, const BIG: bool>(
    bytes: &[u8],
    data64: u64,
    scale: u8,
    digit: u8,
) -> Result<D, D::Error> {
    // we have already validated that we cannot overflow
    let data64 = data64 * 10 + digit as u64;
    let scale = if POINT { scale + 1 } else { 0 };

    if let Some((next, bytes)) = bytes.split_first() {
        let next = *next;
        if POINT && BIG && scale >= D::MAX_SCALE {
            maybe_round(data64 as u128, next, scale, POINT, NEG, bytes)
        } else if BIG && overflow_64(data64) {
            handle_full_128::<D, POINT, NEG>(data64 as u128, bytes, scale, next)
        } else {
            byte_dispatch_u64::<D, POINT, NEG, true, BIG, false>(bytes, data64, scale, next)
        }
    } else {
        let data: u128 = data64 as u128;

        handle_data::<D, NEG, true>(data, scale)
    }
}

#[inline(never)]
fn handle_point<D: StrParser, const NEG: bool, const HAS: bool, const BIG: bool>(
    bytes: &[u8],
    data64: u64,
    scale: u8,
) -> Result<D, D::Error> {
    dispatch_next::<D, true, NEG, HAS, BIG>(bytes, data64, scale)
}

#[inline(never)]
fn handle_separator<D: StrParser, const POINT: bool, const NEG: bool, const BIG: bool>(
    bytes: &[u8],
    data64: u64,
    scale: u8,
) -> Result<D, D::Error> {
    dispatch_next::<D, POINT, NEG, true, BIG>(bytes, data64, scale)
}

#[inline(never)]
#[cold]
fn tail_invalid_digit<D: StrParser>(digit: u8) -> Result<D, D::Error> {
    match digit {
        b'.' => tail_error("Invalid decimal: two decimal points"),
        b'_' => tail_error("Invalid decimal: must start lead with a number"),
        _ => tail_error("Invalid decimal: unknown character"),
    }
}

#[inline(never)]
#[cold]
fn handle_full_128<D: StrParser, const POINT: bool, const NEG: bool>(
    mut data: u128,
    bytes: &[u8],
    scale: u8,
    next_byte: u8,
) -> Result<D, D::Error> {
    let b = next_byte;
    match b {
        b'0'..=b'9' => {
            let digit = u32::from(b - b'0');

            // If the data is going to overflow then we should go into recovery mode
            let next = (data * 10) + digit as u128;
            if D::overflows_at(next, scale) {
                if !POINT {
                    return tail_error("Invalid decimal: overflow from too many digits");
                }

                if digit >= 5 {
                    data += 1;
                }
                // validate remaining bytes - must handle exponential in here
                let mut point = POINT;
                let mut bytes = bytes;
                while let Some((b, rest)) = bytes.split_first() {
                    bytes = rest;
                    match *b {
                        b'0'..=b'9' => (),
                        b'.' if !point => point = true,
                        b'_' => (),
                        b'e' | b'E' if D::HANDLES_SCIENTIFIC => return handle_scientific::<D, NEG>(bytes, data, scale),
                        b => return tail_invalid_digit(b),
                    }
                }
                handle_data::<D, NEG, true>(data, scale)
            } else {
                data = next;
                let scale = scale + POINT as u8;
                if let Some((next, bytes)) = bytes.split_first() {
                    let next = *next;
                    if POINT && scale >= D::MAX_SCALE {
                        maybe_round(data, next, scale, POINT, NEG, bytes)
                    } else {
                        handle_full_128::<D, POINT, NEG>(data, bytes, scale, next)
                    }
                } else {
                    handle_data::<D, NEG, true>(data, scale)
                }
            }
        }
        b'.' if !POINT => {
            // This call won't tail?
            if let Some((next, bytes)) = bytes.split_first() {
                handle_full_128::<D, true, NEG>(data, bytes, scale, *next)
            } else {
                handle_data::<D, NEG, true>(data, scale)
            }
        }
        b'_' => {
            if let Some((next, bytes)) = bytes.split_first() {
                handle_full_128::<D, POINT, NEG>(data, bytes, scale, *next)
            } else {
                handle_data::<D, NEG, true>(data, scale)
            }
        }
        b'e' | b'E' if D::HANDLES_SCIENTIFIC => handle_scientific::<D, NEG>(bytes, data, scale),
        b => tail_invalid_digit(b),
    }
}

#[inline(never)]
#[cold]
fn maybe_round<D: StrParser>(
    mut data: u128,
    next_byte: u8,
    scale: u8,
    mut point: bool,
    negative: bool,
    rest: &[u8],
) -> Result<D, D::Error> {
    let mut scientific = false;
    let mut digit = match next_byte {
        b'0'..=b'9' => Some(u32::from(next_byte - b'0')),
        b'_' => None, // this is accepted by the rust parser
        b'.' if !point => {
            point = true;
            None
        }
        b'e' | b'E' if D::HANDLES_SCIENTIFIC => {
            scientific = true;
            None
        }
        b => return tail_invalid_digit(b),
    };

    // We 1. Might need to recover a digit
    // 2. Need to detect invalid digits off the end
    let mut bytes = rest;
    if !scientific {
        while let Some((b, rest)) = bytes.split_first() {
            bytes = rest;
            match *b {
                b'0'..=b'9' => {
                    if digit.is_none() {
                        digit = Some(u32::from(*b - b'0'));
                    }
                }
                b'.' if !point => point = true,
                b'_' => (),
                b'e' | b'E' if D::HANDLES_SCIENTIFIC => {
                    scientific = true;
                    break;
                }
                b => return tail_invalid_digit(b),
            }
        }
    }
    let digit = digit.unwrap_or(0);

    // Round at midpoint
    if digit >= 5 {
        data += 1;
        if D::overflows_at(data, scale) {
            // Highly unlikely scenario which is more indicative of a bug
            return tail_error("Invalid decimal: overflow when rounding");
        }
    }

    if !scientific {
        debug_assert_eq!(bytes.len(), 0);
    }

    match (scientific, negative) {
        (true, true) => handle_scientific::<D, true>(bytes, data, scale),
        (true, false) => handle_scientific::<D, false>(bytes, data, scale),
        (false, true) => handle_data::<D, true, true>(data, scale),
        (false, false) => handle_data::<D, false, true>(data, scale),
    }
}

#[inline(never)]
fn tail_no_has<D: StrParser>() -> Result<D, D::Error> {
    tail_error("Invalid decimal: no digits found")
}

#[inline]
fn handle_data<D: StrParser, const NEG: bool, const HAS: bool>(data: u128, scale: u8) -> Result<D, D::Error> {
    if !HAS {
        tail_no_has()
    } else {
        D::from_scale(data, scale, NEG)
    }
}

#[inline(never)]
fn handle_scientific<D: StrParser, const NEG: bool>(bytes: &[u8], data: u128, scale: u8) -> Result<D, D::Error> {
    let exp_str =
        core::str::from_utf8(bytes).map_err(|_| D::error_from_string("non-ascii given to handle_scientific"))?;

    if let Ok(exp) = exp_str.parse() {
        D::from_scientific(data, scale, NEG, exp)
    } else {
        tail_error("Exponent could not be parsed")
    }
}

pub(crate) fn parse_str_radix_n(str: &str, radix: u32) -> Result<Decimal, crate::Error> {
    if str.is_empty() {
        return Err(Error::from("Invalid decimal: empty"));
    }
    if radix < 2 {
        return Err(Error::from("Unsupported radix < 2"));
    }
    if radix > 36 {
        // As per trait documentation
        return Err(Error::from("Unsupported radix > 36"));
    }

    let mut offset = 0;
    let mut len = str.len();
    let bytes = str.as_bytes();
    let mut negative = false; // assume positive

    // handle the sign
    if bytes[offset] == b'-' {
        negative = true; // leading minus means negative
        offset += 1;
        len -= 1;
    } else if bytes[offset] == b'+' {
        // leading + allowed
        offset += 1;
        len -= 1;
    }

    // should now be at numeric part of the significand
    let mut digits_before_dot: i32 = -1; // digits before '.', -1 if no '.'
    let mut coeff = ArrayVec::<_, 96>::new(); // integer significand array

    // Supporting different radix
    let (max_n, max_alpha_lower, max_alpha_upper) = if radix <= 10 {
        (b'0' + (radix - 1) as u8, 0, 0)
    } else {
        let adj = (radix - 11) as u8;
        (b'9', adj + b'a', adj + b'A')
    };

    // Estimate the max precision. All in all, it needs to fit into 96 bits.
    // Rather than try to estimate, I've included the constants directly in here. We could,
    // perhaps, replace this with a formula if it's faster - though it does appear to be log2.
    let estimated_max_precision = match radix {
        2 => 96,
        3 => 61,
        4 => 48,
        5 => 42,
        6 => 38,
        7 => 35,
        8 => 32,
        9 => 31,
        10 => 28,
        11 => 28,
        12 => 27,
        13 => 26,
        14 => 26,
        15 => 25,
        16 => 24,
        17 => 24,
        18 => 24,
        19 => 23,
        20 => 23,
        21 => 22,
        22 => 22,
        23 => 22,
        24 => 21,
        25 => 21,
        26 => 21,
        27 => 21,
        28 => 20,
        29 => 20,
        30 => 20,
        31 => 20,
        32 => 20,
        33 => 20,
        34 => 19,
        35 => 19,
        36 => 19,
        _ => return Err(Error::from("Unsupported radix")),
    };

    let mut maybe_round = false;
    while len > 0 {
        let b = bytes[offset];
        match b {
            b'0'..=b'9' => {
                if b > max_n {
                    return Err(Error::from("Invalid decimal: invalid character"));
                }
                coeff.push(u32::from(b - b'0'));
                offset += 1;
                len -= 1;

                // If the coefficient is longer than the max, exit early
                if coeff.len() as u32 > estimated_max_precision {
                    maybe_round = true;
                    break;
                }
            }
            b'a'..=b'z' => {
                if b > max_alpha_lower {
                    return Err(Error::from("Invalid decimal: invalid character"));
                }
                coeff.push(u32::from(b - b'a') + 10);
                offset += 1;
                len -= 1;

                if coeff.len() as u32 > estimated_max_precision {
                    maybe_round = true;
                    break;
                }
            }
            b'A'..=b'Z' => {
                if b > max_alpha_upper {
                    return Err(Error::from("Invalid decimal: invalid character"));
                }
                coeff.push(u32::from(b - b'A') + 10);
                offset += 1;
                len -= 1;

                if coeff.len() as u32 > estimated_max_precision {
                    maybe_round = true;
                    break;
                }
            }
            b'.' => {
                if digits_before_dot >= 0 {
                    return Err(Error::from("Invalid decimal: two decimal points"));
                }
                digits_before_dot = coeff.len() as i32;
                offset += 1;
                len -= 1;
            }
            b'_' => {
                // Must start with a number...
                if coeff.is_empty() {
                    return Err(Error::from("Invalid decimal: must start lead with a number"));
                }
                offset += 1;
                len -= 1;
            }
            _ => return Err(Error::from("Invalid decimal: unknown character")),
        }
    }

    // If we exited before the end of the string then do some rounding if necessary
    if maybe_round && offset < bytes.len() {
        let next_byte = bytes[offset];
        let digit = match next_byte {
            b'0'..=b'9' => {
                if next_byte > max_n {
                    return Err(Error::from("Invalid decimal: invalid character"));
                }
                u32::from(next_byte - b'0')
            }
            b'a'..=b'z' => {
                if next_byte > max_alpha_lower {
                    return Err(Error::from("Invalid decimal: invalid character"));
                }
                u32::from(next_byte - b'a') + 10
            }
            b'A'..=b'Z' => {
                if next_byte > max_alpha_upper {
                    return Err(Error::from("Invalid decimal: invalid character"));
                }
                u32::from(next_byte - b'A') + 10
            }
            b'_' => 0,
            b'.' => {
                // Still an error if we have a second dp
                if digits_before_dot >= 0 {
                    return Err(Error::from("Invalid decimal: two decimal points"));
                }
                0
            }
            _ => return Err(Error::from("Invalid decimal: unknown character")),
        };

        // Round at midpoint
        let midpoint = if radix & 0x1 == 1 { radix / 2 } else { (radix + 1) / 2 };
        if digit >= midpoint {
            let mut index = coeff.len() - 1;
            loop {
                let new_digit = coeff[index] + 1;
                if new_digit <= 9 {
                    coeff[index] = new_digit;
                    break;
                } else {
                    coeff[index] = 0;
                    if index == 0 {
                        coeff.insert(0, 1u32);
                        digits_before_dot += 1;
                        coeff.pop();
                        break;
                    }
                }
                index -= 1;
            }
        }
    }

    // here when no characters left
    if coeff.is_empty() {
        return Err(Error::from("Invalid decimal: no digits found"));
    }

    let mut scale = if digits_before_dot >= 0 {
        // we had a decimal place so set the scale
        (coeff.len() as u32) - (digits_before_dot as u32)
    } else {
        0
    };

    // Parse this using specified radix
    let mut data = [0u32, 0u32, 0u32];
    let mut tmp = [0u32, 0u32, 0u32];
    let len = coeff.len();
    for (i, digit) in coeff.iter().enumerate() {
        // If the data is going to overflow then we should go into recovery mode
        tmp[0] = data[0];
        tmp[1] = data[1];
        tmp[2] = data[2];
        let overflow = mul_by_u32(&mut tmp, radix);
        if overflow > 0 {
            // This means that we have more data to process, that we're not sure what to do with.
            // This may or may not be an issue - depending on whether we're past a decimal point
            // or not.
            if (i as i32) < digits_before_dot && i + 1 < len {
                return Err(Error::from("Invalid decimal: overflow from too many digits"));
            }

            if *digit >= 5 {
                let carry = add_one_internal(&mut data);
                if carry > 0 {
                    // Highly unlikely scenario which is more indicative of a bug
                    return Err(Error::from("Invalid decimal: overflow when rounding"));
                }
            }
            // We're also one less digit so reduce the scale
            let diff = (len - i) as u32;
            if diff > scale {
                return Err(Error::from("Invalid decimal: overflow from scale mismatch"));
            }
            scale -= diff;
            break;
        } else {
            data[0] = tmp[0];
            data[1] = tmp[1];
            data[2] = tmp[2];
            let carry = add_by_internal_flattened(&mut data, *digit);
            if carry > 0 {
                // Highly unlikely scenario which is more indicative of a bug
                return Err(Error::from("Invalid decimal: overflow from carry"));
            }
        }
    }

    Ok(Decimal::from_parts(data[0], data[1], data[2], negative, scale))
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::Decimal;
    use arrayvec::ArrayString;
    use core::{fmt::Write, str::FromStr};

    #[test]
    fn display_does_not_overflow_max_capacity() {
        let num = Decimal::from_str("1.2").unwrap();
        let mut buffer = ArrayString::<64>::new();
        buffer.write_fmt(format_args!("{:.31}", num)).unwrap();
        assert_eq!("1.2000000000000000000000000000000", buffer.as_str());
    }

    #[test]
    fn from_str_rounding_0() {
        assert_eq!(
            parse_str_radix_10("1.234").unwrap().unpack(),
            Decimal::new(1234, 3).unpack()
        );
    }

    #[test]
    fn from_str_rounding_1() {
        assert_eq!(
            parse_str_radix_10("11111_11111_11111.11111_11111_11111")
                .unwrap()
                .unpack(),
            Decimal::from_i128_with_scale(11_111_111_111_111_111_111_111_111_111, 14).unpack()
        );
    }

    #[test]
    fn from_str_rounding_many_zeros() {
        assert_eq!(
            parse_str_radix_10("11111_11111_11111.11111_11111_111110000000000000000000000000000000")
                .unwrap()
                .unpack(),
            Decimal::from_i128_with_scale(11_111_111_111_111_111_111_111_111_111, 14).unpack()
        );
    }

    #[test]
    fn from_str_rounding_invalid_at_end() {
        assert!(parse_str_radix_10("11111_11111_11111.11111_11111_111110000000000000000000000000000000-").is_err());
    }

    #[test]
    fn from_str_rounding_2() {
        assert_eq!(
            parse_str_radix_10("11111_11111_11111.11111_11111_11115")
                .unwrap()
                .unpack(),
            Decimal::from_i128_with_scale(11_111_111_111_111_111_111_111_111_112, 14).unpack()
        );
    }

    #[test]
    fn from_str_rounding_3() {
        assert_eq!(
            parse_str_radix_10("11111_11111_11111.11111_11111_11195")
                .unwrap()
                .unpack(),
            Decimal::from_i128_with_scale(11_111_111_111_111_111_111_111_111_120, 14).unpack() // was Decimal::from_i128_with_scale(1_111_111_111_111_111_111_111_111_112, 13)
        );
    }

    #[test]
    fn from_str_rounding_4() {
        assert_eq!(
            parse_str_radix_10("99999_99999_99999.99999_99999_99995")
                .unwrap()
                .unpack(),
            Decimal::from_i128_with_scale(10_000_000_000_000_000_000_000_000_000, 13).unpack() // was Decimal::from_i128_with_scale(1_000_000_000_000_000_000_000_000_000, 12)
        );
    }

    #[test]
    fn from_str_many_pointless_chars() {
        assert_eq!(
            parse_str_radix_10("00________________________________________________________________001.1")
                .unwrap()
                .unpack(),
            Decimal::from_i128_with_scale(11, 1).unpack()
        );
    }

    #[test]
    fn from_str_leading_0s_1() {
        assert_eq!(
            parse_str_radix_10("00001.1").unwrap().unpack(),
            Decimal::from_i128_with_scale(11, 1).unpack()
        );
    }

    #[test]
    fn from_str_leading_0s_2() {
        assert_eq!(
            parse_str_radix_10("00000_00000_00000_00000_00001.00001")
                .unwrap()
                .unpack(),
            Decimal::from_i128_with_scale(100001, 5).unpack()
        );
    }

    #[test]
    fn from_str_leading_0s_3() {
        assert_eq!(
            parse_str_radix_10("0.00000_00000_00000_00000_00000_00100")
                .unwrap()
                .unpack(),
            Decimal::from_i128_with_scale(1, 28).unpack()
        );
    }

    #[test]
    fn from_str_trailing_0s_1() {
        assert_eq!(
            parse_str_radix_10("0.00001_00000_00000").unwrap().unpack(),
            Decimal::from_i128_with_scale(10_000_000_000, 15).unpack()
        );
    }

    #[test]
    fn from_str_trailing_0s_2() {
        assert_eq!(
            parse_str_radix_10("0.00001_00000_00000_00000_00000_00000")
                .unwrap()
                .unpack(),
            Decimal::from_i128_with_scale(100_000_000_000_000_000_000_000, 28).unpack()
        );
    }

    #[test]
    fn from_str_overflow_1() {
        assert_eq!(
            parse_str_radix_10("99999_99999_99999_99999_99999_99999.99999"),
            // The original implementation returned
            //              Ok(10000_00000_00000_00000_00000_0000)
            // Which is a bug!
            Err(Error::from("Invalid decimal: overflow from too many digits"))
        );
    }

    #[test]
    fn from_str_overflow_2() {
        assert!(
            parse_str_radix_10("99999_99999_99999_99999_99999_11111.11111").is_err(),
            // The original implementation is 'overflow from scale mismatch'
            // but we got rid of that now
        );
    }

    #[test]
    fn from_str_overflow_3() {
        assert!(
            parse_str_radix_10("99999_99999_99999_99999_99999_99994").is_err() // We could not get into 'overflow when rounding' or 'overflow from carry'
                                                                               // in the original implementation because the rounding logic before prevented it
        );
    }

    #[test]
    fn from_str_overflow_4() {
        assert_eq!(
            // This does not overflow, moving the decimal point 1 more step would result in
            // 'overflow from too many digits'
            parse_str_radix_10("99999_99999_99999_99999_99999_999.99")
                .unwrap()
                .unpack(),
            Decimal::from_i128_with_scale(10_000_000_000_000_000_000_000_000_000, 0).unpack()
        );
    }

    #[test]
    fn from_str_edge_cases_1() {
        assert_eq!(parse_str_radix_10(""), Err(Error::from("Invalid decimal: empty")));
    }

    #[test]
    fn from_str_edge_cases_2() {
        assert_eq!(
            parse_str_radix_10("0.1."),
            Err(Error::from("Invalid decimal: two decimal points"))
        );
    }

    #[test]
    fn from_str_edge_cases_3() {
        assert_eq!(
            parse_str_radix_10("_"),
            Err(Error::from("Invalid decimal: must start lead with a number"))
        );
    }

    #[test]
    fn from_str_edge_cases_4() {
        assert_eq!(
            parse_str_radix_10("1?2"),
            Err(Error::from("Invalid decimal: unknown character"))
        );
    }

    #[test]
    fn from_str_edge_cases_5() {
        assert_eq!(
            parse_str_radix_10("."),
            Err(Error::from("Invalid decimal: no digits found"))
        );
    }
}

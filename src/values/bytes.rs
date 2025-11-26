/// Python bytes type, wrapping a `Vec<u8>`.
///
/// This type provides Python bytes semantics. Currently supports basic
/// operations like length and equality comparison.
use std::borrow::Cow;

use crate::exceptions::ExcType;
use crate::heap::{Heap, ObjectId};
use crate::object::{Attr, Object};
use crate::run::RunResult;
use crate::values::PyValue;

/// Python bytes value stored on the heap.
///
/// Wraps a `Vec<u8>` and provides Python-compatible operations.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Bytes(Vec<u8>);

impl Bytes {
    /// Creates a new Bytes from a byte vector.
    #[must_use]
    pub fn new(bytes: Vec<u8>) -> Self {
        Self(bytes)
    }

    /// Returns a reference to the inner byte slice.
    #[must_use]
    pub fn as_slice(&self) -> &[u8] {
        &self.0
    }

    /// Returns a mutable reference to the inner byte vector.
    pub fn as_vec_mut(&mut self) -> &mut Vec<u8> {
        &mut self.0
    }
}

impl From<Vec<u8>> for Bytes {
    fn from(bytes: Vec<u8>) -> Self {
        Self(bytes)
    }
}

impl From<&[u8]> for Bytes {
    fn from(bytes: &[u8]) -> Self {
        Self(bytes.to_vec())
    }
}

impl From<Bytes> for Vec<u8> {
    fn from(bytes: Bytes) -> Self {
        bytes.0
    }
}

impl std::ops::Deref for Bytes {
    type Target = Vec<u8>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl PyValue for Bytes {
    fn py_type(&self, _heap: &Heap) -> &'static str {
        "bytes"
    }

    fn py_len(&self, _heap: &Heap) -> Option<usize> {
        Some(self.0.len())
    }

    fn py_eq(&self, other: &Self, _heap: &Heap) -> bool {
        self.0 == other.0
    }

    /// Bytes don't contain nested heap references.
    fn py_dec_ref_ids(&self, _stack: &mut Vec<ObjectId>) {
        // No-op: bytes don't hold Object references
    }

    fn py_bool(&self, _heap: &Heap) -> bool {
        !self.0.is_empty()
    }

    fn py_repr<'h>(&'h self, _heap: &'h Heap) -> Cow<'h, str> {
        Cow::Owned(format!("b'{:?}'", self.0.as_slice()))
    }

    fn py_call_attr<'c>(&mut self, heap: &mut Heap, attr: &Attr, _args: Vec<Object>) -> RunResult<'c, Object> {
        Err(ExcType::attribute_error(self.py_type(heap), attr))
    }
}

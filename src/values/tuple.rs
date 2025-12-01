/// Python tuple type, wrapping a `Vec<Object<'e>>`.
///
/// This type provides Python tuple semantics. Tuples are immutable sequences
/// that can contain any Python object. Like lists, tuples properly handle
/// reference counting for heap-allocated objects.
use std::borrow::Cow;

use crate::args::ArgObjects;
use crate::exceptions::ExcType;
use crate::heap::{Heap, ObjectId};
use crate::object::{Attr, Object};
use crate::run::RunResult;
use crate::values::list::repr_sequence;
use crate::values::PyValue;

/// Python tuple value stored on the heap.
///
/// Wraps a `Vec<Object<'e>>` and provides Python-compatible tuple operations.
/// Unlike lists, tuples are conceptually immutable (though this is not
/// enforced at the type level for internal operations).
///
/// # Reference Counting
/// When a tuple is freed, all contained heap references have their refcounts
/// decremented via `push_stack_ids`.
#[derive(Debug, PartialEq, Default)]
pub struct Tuple<'e>(Vec<Object<'e>>);

impl<'e> Tuple<'e> {
    /// Creates a new tuple from a vector of objects.
    ///
    /// Note: This does NOT increment reference counts - the caller must
    /// ensure refcounts are properly managed.
    #[must_use]
    pub fn from_vec(vec: Vec<Object<'e>>) -> Self {
        Self(vec)
    }

    /// Returns a reference to the underlying vector.
    #[must_use]
    pub fn as_vec(&self) -> &Vec<Object<'e>> {
        &self.0
    }

    /// Creates a deep clone of this tuple with proper reference counting.
    ///
    /// All heap-allocated objects in the tuple have their reference counts
    /// incremented. This should be used instead of `.clone()` which would
    /// bypass reference counting.
    #[must_use]
    pub fn clone_with_heap(&self, heap: &mut Heap<'e>) -> Self {
        let cloned: Vec<Object<'e>> = self.0.iter().map(|obj| obj.clone_with_heap(heap)).collect();
        Self(cloned)
    }
}

impl<'e> From<Vec<Object<'e>>> for Tuple<'e> {
    fn from(vec: Vec<Object<'e>>) -> Self {
        Self(vec)
    }
}

impl<'e> std::iter::FromIterator<Object<'e>> for Tuple<'e> {
    fn from_iter<I: IntoIterator<Item = Object<'e>>>(iter: I) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl<'e> From<Tuple<'e>> for Vec<Object<'e>> {
    fn from(tuple: Tuple<'e>) -> Self {
        tuple.0
    }
}

impl<'e> PyValue<'e> for Tuple<'e> {
    fn py_type(&self, _heap: &Heap<'e>) -> &'static str {
        "tuple"
    }

    fn py_len(&self, _heap: &Heap<'e>) -> Option<usize> {
        Some(self.0.len())
    }

    fn py_getitem(&self, key: &Object<'e>, heap: &mut Heap<'e>) -> RunResult<'static, Object<'e>> {
        // Extract integer index from key, returning TypeError if not an int
        let index = match key {
            Object::Int(i) => *i,
            _ => return Err(ExcType::type_error_indices("tuple", key.py_type(heap))),
        };

        // Convert to usize, handling negative indices (Python-style: -1 = last element)
        let len = self.0.len() as i64;
        let normalized_index = if index < 0 { index + len } else { index };

        // Bounds check
        if normalized_index < 0 || normalized_index >= len {
            return Err(ExcType::tuple_index_error());
        }

        // Return clone of the item with proper refcount increment
        Ok(self.0[normalized_index as usize].clone_with_heap(heap))
    }

    fn py_eq(&self, other: &Self, heap: &mut Heap<'e>) -> bool {
        if self.0.len() != other.0.len() {
            return false;
        }
        for (i1, i2) in self.0.iter().zip(&other.0) {
            if !i1.py_eq(i2, heap) {
                return false;
            }
        }
        true
    }

    /// Pushes all heap object IDs contained in this tuple onto the stack.
    ///
    /// Called during garbage collection to decrement refcounts of nested objects.
    fn py_dec_ref_ids(&self, stack: &mut Vec<ObjectId>) {
        for obj in &self.0 {
            if let Object::Ref(id) = obj {
                stack.push(*id);
            }
        }
    }

    /// Tuples don't support attribute calls.
    fn py_call_attr(
        &mut self,
        heap: &mut Heap<'e>,
        attr: &Attr,
        _args: ArgObjects<'e>,
    ) -> RunResult<'static, Object<'e>> {
        Err(ExcType::attribute_error(self.py_type(heap), attr))
    }

    fn py_bool(&self, _heap: &Heap<'e>) -> bool {
        !self.0.is_empty()
    }

    fn py_repr<'a>(&'a self, heap: &'a Heap<'e>) -> Cow<'a, str> {
        Cow::Owned(repr_sequence('(', ')', &self.0, heap))
    }
}

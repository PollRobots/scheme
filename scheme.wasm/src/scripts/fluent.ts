export function* take<T>(iter: Iterator<T>, count: number): Generator<T> {
  for (let i = 0; i < count; i++) {
    const curr = iter.next();
    if (curr.done) {
      return;
    }
    yield curr.value;
  }
}

export function* skip<T>(iter: Iterator<T>, count: number): Generator<T> {
  for (let i = 0; i < count; i++) {
    const curr = iter.next();
    if (curr.done) {
      return;
    }
  }
  while (true) {
    const curr = iter.next();
    if (curr.done) {
      return;
    }
    yield curr.value;
  }
}

export function every<T>(
  iter: Iterable<T>,
  predicate: (el: T) => boolean
): boolean {
  for (const el of iter) {
    if (!predicate(el)) {
      return false;
    }
  }
  return true;
}

export function some<T>(
  iter: Iterable<T>,
  predicate: (el: T) => boolean
): boolean {
  for (const el of iter) {
    if (predicate(el)) {
      return true;
    }
  }
  return false;
}

export function empty<T>(iter: Iterable<T>): boolean {
  return !!iter[Symbol.iterator]().next().done;
}

export function* map<T, V>(
  iter: Iterable<T>,
  mapFn: (el: T) => V
): Generator<V> {
  for (const el of iter) {
    yield mapFn(el);
  }
}

export function* filter<T>(
  iter: Iterable<T>,
  predicate: (el: T) => boolean
): Generator<T> {
  for (const el of iter) {
    if (predicate(el)) {
      yield el;
    }
  }
}

export function* zip<T1, T2>(
  left: Iterable<T1>,
  right: Iterable<T2>
): Generator<[T1, T2]> {
  const leftIter = left[Symbol.iterator]();
  const rightIter = right[Symbol.iterator]();
  while (true) {
    const currLeft = leftIter.next();
    const currRight = rightIter.next();
    if (currLeft.done || currRight.done) {
      return;
    }
    yield [currLeft.value, currRight.value];
  }
}

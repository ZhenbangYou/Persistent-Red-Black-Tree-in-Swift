@frozen
enum Color {
  case R
  case B
}

@frozen
indirect enum Tree<A> {
  case E
  case T(Color, Tree<A>, A, Tree<A>)
}

func member<A: Comparable>(item x: A, tree: Tree<A>) -> Bool {
  switch tree {
  case .E: return false
  case let .T(_, a, y, b):
    if x < y {
      return member(item: x, tree: a)
    } else if x == y {
      return true
    } else {
      return member(item: x, tree: b)
    }
  }
}

func balance<A: Comparable>(color: Color, leftChild: Tree<A>, mid: A, rightChild: Tree<A>) -> Tree<
  A
> {
  switch (color, leftChild, mid, rightChild) {
  case let (.B, .T(.R, .T(.R, a, x, b), y, c), z, d):
    return .T(.R, .T(.B, a, x, b), y, .T(.B, c, z, d))
  case let (.B, .T(.R, a, x, .T(.R, b, y, c)), z, d):
    return .T(.R, .T(.B, a, x, b), y, .T(.B, c, z, d))
  case let (.B, a, x, .T(.R, .T(.R, b, y, c), z, d)):
    return .T(.R, .T(.B, a, x, b), y, .T(.B, c, z, d))
  case let (.B, a, x, .T(.R, b, y, .T(.R, c, z, d))):
    return .T(.R, .T(.B, a, x, b), y, .T(.B, c, z, d))
  default:
    return .T(color, leftChild, mid, rightChild)
  }
}

func insert<A: Comparable>(item x: A, tree s: Tree<A>) -> Tree<A> {
  func ins(_ it: Tree<A>) -> Tree<A> {
    switch it {
    case .E:
      return .T(.R, .E, x, .E)
    case let .T(color, a, y, b):
      if x < y {
        return balance(color: color, leftChild: ins(a), mid: y, rightChild: b)
      } else if x == y {
        return .T(color, a, y, b)
      } else {
        return balance(color: color, leftChild: a, mid: y, rightChild: ins(b))
      }
    }
  }
  func makeBlack(innerTree it: Tree<A>) -> Tree<A> {
    switch it {
    case let .T(_, a, y, b):
      return .T(.B, a, y, b)
    default:
      fatalError("diverge in insert")
    }
  }
  return makeBlack(innerTree: ins(s))
}

func del<A: Comparable>(_ x: A, _ t: Tree<A>) -> Tree<A> {
  switch t {
  case let .T(_, l, y, r):
    if x < y {
      return delL(x, t)
    } else if x > y {
      return delR(x, t)
    } else {
      return fuse(l, r)
    }
  default:
    fatalError("diverge in del")
  }
}

func balL<A: Comparable>(_ tree: Tree<A>) -> Tree<A> {
  switch tree {
  case let .T(.B, .T(.R, t1, x, t2), y, t3):
    return .T(.R, .T(.B, t1, x, t2), y, t3)
  case let .T(.B, t1, y, .T(.B, t2, z, t3)):
    return balance(color: .B, leftChild: t1, mid: y, rightChild: .T(.R, t2, z, t3))
  case let .T(.B, t1, y, .T(.R, .T(.B, t2, u, t3), z, t4)):
    switch t4 {
    case let .T(.B, l, value, r):
      return .T(
        .R, .T(.B, t1, y, t2), u,
        balance(color: .B, leftChild: t3, mid: z, rightChild: .T(.R, l, value, r)))
    default: fatalError("diverge in balL inner")
    }
  default: fatalError("diverge in balL outer")
  }
}

func balR<A: Comparable>(_ tree: Tree<A>) -> Tree<A> {
  switch tree {
  case let .T(.B, t1, y, .T(.R, t2, x, t3)):
    return .T(.R, t1, y, .T(.B, t2, x, t3))
  case let .T(.B, .T(.B, t1, z, t2), y, t3):
    return balance(color: .B, leftChild: .T(.R, t1, z, t2), mid: y, rightChild: t3)
  case let .T(.B, .T(.R, t1, z, .T(.B, t2, u, t3)), y, t4):
    switch t1 {
    case let .T(.B, l, value, r):
      return .T(
        .R, balance(color: .B, leftChild: .T(.R, l, value, r), mid: z, rightChild: t2), u,
        .T(.B, t3, y, t4))
    default: fatalError("diverge in balR inner")
    }
  default: fatalError("diverge in balR outer")
  }
}

func delL<A: Comparable>(_ x: A, _ t: Tree<A>) -> Tree<A> {
  switch t {
  case let .T(.B, t1, y, t2):
    return balL(.T(.B, del(x, t1), y, t2))
  case let .T(.R, t1, y, t2):
    return .T(.R, del(x, t1), y, t2)
  default:
    fatalError("diverge in del")
  }
}

func delR<A: Comparable>(_ x: A, _ t: Tree<A>) -> Tree<A> {
  switch t {
  case let .T(.B, t1, y, t2):
    return balR(.T(.B, t1, y, del(x, t2)))
  case let .T(.R, t1, y, t2):
    return .T(.R, t1, y, del(x, t2))
  default:
    fatalError("diverge in del")
  }
}

func fuse<A: Comparable>(_ tree1: Tree<A>, _ tree2: Tree<A>) -> Tree<A> {
  switch (tree1, tree2) {
  case let (.E, t):
    return t
  case let (t, .E):
    return t
  case let (.T(.R, t1, x, t2), .T(.R, t3, y, t4)):
    let s = fuse(t2, t3)
    switch s {
    case let .T(.R, s1, z, s2):
      return .T(.R, .T(.R, t1, x, s1), z, .T(.R, s2, y, t4))
    case .T(.B, _, _, _):
      return .T(.R, t1, x, .T(.R, s, y, t4))
    default:
      fatalError("diverge in fuse")
    }
  case let (.T(.B, t1, x, t2), .T(.B, t3, y, t4)):
    let s = fuse(t2, t3)
    switch s {
    case let .T(.R, s1, z, s2):
      return .T(.R, .T(.R, t1, x, s1), z, .T(.R, s2, y, t4))
    case .T(.B, _, _, _):
      return balL(.T(.B, t1, x, .T(.B, s, y, t4)))
    default:
      fatalError("diverge in fuse")
    }
  case let (t1, .T(.R, t3, y, t4)):
    switch t1 {
    case .T(.B, _, _, _):
      return .T(.R, fuse(t1, t3), y, t4)
    default:
      fatalError("diverge in fuse")
    }
  case let (.T(.R, t1, x, t2), t3):
    switch t3 {
    case .T(.B, _, _, _):
      return .T(.R, t1, x, fuse(t2, t3))
    default:
      fatalError("diverge in fuse")
    }
  default:
    fatalError("diverge in fuse")
  }
}

func delete<A: Comparable>(item x: A, tree t: Tree<A>) -> Tree<A> {
  func makeBlack(_ it: Tree<A>) -> Tree<A> {
    switch it {
    case let .T(_, a, y, b):
      return .T(.B, a, y, b)
    default:
      fatalError("diverge in insert")
    }
  }
  return makeBlack(del(x, t))
}

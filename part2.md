- [第二部分：<ruby>食用猫呢<rt> Practical Monads</rt></ruby>](#org36fbf26)
  - [Identity](#orge020998)
  - [Maybe](#org9fa09f1)
  - [Either](#org377efa7)
    - [Product & Coproduct](#orgf797368)
    - [Either Monad](#org2c35fd6)
  - [Validate](#orgd617b22)
  - [Reader](#org2f34246)
    - [do notation](#orgd9a95d4)
  - [Writer](#orgaa3371f)
  - [State](#org0995559)
  - [Expection](#org3638020)
  - [Cont](#orgff354b5)
  - [Summary](#org6bebeac)
  - [References](#orgae307f0)

-   [第一部分：范畴论](./index.md)
-   **[第二部分：食用猫呢](./part2.md)** 👈
-   [第三部分：搞基猫呢](./part3.md)

第一部分理论部分都讲完了， 如果你读到这里还没有被这些吊炸天的概念搞daze，接下来可以看看它到底跟我们编程有鸟关系呢？

第二部分将介绍由这些概念产生的一些实用的monad instances，这些 monad 都通过同样的抽象方式，解决了分离计算与副作用的工作。

最后一部分，我们还可以像 IO monad 一样，通过 free 或者 Eff 自定义自己的计算，和可能带副作用的解释器。


<a id="org36fbf26"></a>

# 第二部分：<ruby>食用猫呢<rt> Practical Monads</rt></ruby>

一些有用的 Monad instances


<a id="orge020998"></a>

## Identity

这可能是最简单的 monad 了。不包含任何计算

```haskell
newtype Identity a = Identity { runIdentity :: a }
```

这里使用 `newtype` 而不是 `data` 是因为 `Identity` 与 `runIdentity` 是 isomorphic （同构，忘了的话回去翻第一部分）

```haskell
Identity :: a -> Identity a
runIdentity :: Identity a -> a
```

所以 `runIdentity . Identity = id` ，所以他们是同构的。

左边的 `Identity` 是类型构造器, 接收类型 `a` 返回 `Identity a` 类型

如果 a 是 Int，那么就得到一个 `Identity Int` 类型。

右边的 `Identity` 是数据构造器，也就是构造值，比如 `Identity 1` 会构造出一个值，其类型为 `Identity Int`

大括号比较诡异，可以想象成给 `a` 一个 key，同过这个 key 可以把 a 取出来，比如

```haskell
runIdentity (Identity 1)
```

会返回 1

Identity 可以实现 Functor 和 Monad，就得到 Identity functor 和 Identity monad

```haskell
instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Monad Identity where
  return a = Identity a
  Identity a >>= f = f a
```

可以看到 Identity 即是构造器，也是解构器，在模式匹配是可以 destructure 值。例如上面Functor 实现中的 `fmap f (Identity a)` , 假如fmap的是 `Identity 1`, 那么这个模式匹配到 `(Identity a)` 时会把 `1` 放到 `a` 的位置。

Identity 看起来什么也没有干，就跟 identity 函数一样，但是在后面讲到 State monad时你会发现他的价值。


<a id="org9fa09f1"></a>

## Maybe

这是一个超级简单的 Monad，首先，需要定义这个一个 <ruby>代数数据类型<rt> Algebra Data Type(ADT)</rt></ruby>

```haskell
data Maybe a = Just a | Nothing
```

Haskell中定义一个ADT十分简单，不像Scala那么啰嗦。左边是类型构造器，右边有数据构造器，你会发现有一根竖线 `|` ， 它分隔着两个构造器

-   Just
-   Nothing

其中 a （一定要小写）可以是任意类型

所以 `Just 1` 会得到一个 `Num a => Mabye a` 类型（意思就是 `Maybe a` 但是 `a` 的类型约束为 `Num` ）， `Nothing` 也会得到一个 `Maybe a` 只不过 `a` 没有类型约束。

总之我们有了构造器可以构造出 Maybe 类型，而这个类型能做的事情，就要取决它实现了哪些 class 的 instance 了。比如它可以是一个 Functor

```haskell
instance Functor Maybe where
  fmap f (Just a) = Just (f a)
```

![img](https://www.evernote.com/l/ABeKvGaM2S1LV5QzdmQv_U98pCX164j7-2MB/image.png "fmap :: (a -> b) -> f a -> f b")

然后，还实现 Monad

```haskell
instance Monad Maybe where
  return a = Just a
  (Just a) >>= f = f a
  Nothing >>= f = Nothing
```

![img](https://www.evernote.com/l/ABfAhPgWI_5Awbov_2gLY_BcB9CwKyIJZjYB/image.png "还记得第一部分提到的 Kleisli 范畴吗？")

Maybe 有用在于能合适的处理 *<ruby>偏函数<rt> Partial Function</rt></ruby>* 的返回值。偏函数相对于<ruby>全函数<rt> Total Function</rt></ruby>，是指只能对部分输入返回输出的函数。

比如一个取数组某一位上的值的函数，就是偏函数，因为假设你想取第4位的值，但不是所有数组长度都大于4，就会有获取不了的尴尬情况。

```haskell
[1,2,3] !! 4
```

如果使用 Maybe 把偏函数处理不了的输入都返回成 Nothing，这样结果依然保持 Maybe 类型，不影响后面的计算。


<a id="org377efa7"></a>

## Either

Either 的定义也很简单

```haskell
data Either a b = Left a | Right b
```


<a id="orgf797368"></a>

### Product & Coproduct

看过第一部分应该还能记得有一个东西叫 Duel，所以见到如果范畴上有 Coproduct 那么肯定在duel范畴上会有同样的东西叫 Product。

那么我们先来看看什么是 Coproduct

![img](https://www.evernote.com/l/ABeCtsXrN7xCWqa7bsNAU0eVQVTDdkRKqVEB/image.png "Coproduct")

像这样，能通过两个箭头到达同一个东西，就是 Coproduct。这里箭头 `Left` 能让 `a` 到 `Either a b` ， 箭头 `Right` 也能让 `b` 到达 `Either a b`

有意思的是还肯定存在一个 Coproduct 和 箭头，使得下图成立 ![img](https://www.evernote.com/l/ABfP9Sz8diJFxoXCJpjHeo_gF5JAmsiFvPYB/image.png)

箭头反过来，就是 Product, 比如 Tuple

![img](https://www.evernote.com/l/ABea91BEgH5OH41WorLYjqichYC0rmVCAXMB/image.png "Product")

Tuple 的 `fst` 箭头能让 `(a, b)` 到达 `a` 对象，而箭头 `snd` 能让其到达 `b` 对象。


<a id="org2c35fd6"></a>

### Either Monad

确切的说，Either 不是 monad， `Either a` 才是。还记得 monad 的 class 定义吗？

```haskell
class Endofunctor m => Monad m where
  eta :: a -> (m a)
  mu :: m m a -> m a
```

所以 m 必须是个 Endofunctor，也就是要满足Functor

```haskell
class Functor t where
  fmap :: (a -> b) -> (t a -> t b)
```

t a 的 kind 是 \*，所以 t 必须是 kind \* -> \* 也就是说，m 必须是接收一个类型参数的类型构造器

而 Either 的 kind 是 \* -> \* -> \*, Either a 才是 \* -> \*

所以只能定义 Either a 的 Monad

```haskell
instance Monad (Either a) where
  Left  l >>= _ = Left l
  Right r >>= k = k r
```

很明显的，>>= 任何函数到<ruby>左边<rt> Left</rt></ruby> 都不会改变，只有 >>= 右边才能产生新的计算。


<a id="orgd617b22"></a>

## Validate


<a id="org2f34246"></a>

## Reader

Reader 的作用是给一个计算喂数据。

在描述计算的时候，并不需要关心输入时什么，只需要 asks 就可以拿到输入值

而真正的输入，会在运行计算时给予。

跟 Identity 一样，我们用 newtype 来定义一个同构的 Reader 类型

```haskell
newtype Reader e a = Reader { runReader :: (e -> a) }
```

其中

-   e 是输入
-   a 是结果
-   构造 Reader 类型需要确定 输入的类型 e 与输出的类型 a
-   `runReader` 的类型是 `runReader:: (Reader e a) -> (e -> a)`

也就是说在描述完一个 Reader 的计算后，使用 runReader 可以得到一个 e -> a 的函数，使用这个函数，就可以接收输入，通过构造好的计算，算出结果 a 返回。

那么，让我们来实现 Reader 的 monad instance，就可以描述一个可以 ask 的计算了。

```haskell
instance Monad (Reader e) where 
    return a         = Reader $ \_ -> a 
    (Reader g) >>= f = Reader $ \e -> runReader (f (g e)) e
```

跟Either一样，我们只能定义 Reader e 的 monad instance。

注意这里的

-   f 类型是 `(a -> Reader e a)`
-   g 其实就是是 destructure 出来的 runReader，也就是 e -> a
-   所以 (g e) 返回 a
-   f (g e) 就是 `Reader e a`
-   再 run 一把最后得到 a

![img](https://www.evernote.com/l/ABeL9xOcX7VNmJdaq49OSQf0ejRLsz_EWZ0B/image.png "f 函数，接收 a 返回一个 从 e 到 a 的 Reader")

让我们来看看如何使用 Reader

```haskell
import Control.Monad.Reader

data Environment = Env
  { fistName :: String
  , lastName :: String
  } deriving (Show)

helloworld :: Reader Environment String
helloworld = do
  f <- asks firstName
  l <- asks lastName
  return "Hello " ++ f ++ l

runHelloworld :: String
runHelloworld = runReader helloworld $ Env "Jichao" "Ouyang"
```

这段代码很简单，helloworld 负责打招呼，也就是在名字前面加个 "Hello"，而跟谁打招呼，这个函数并不关心，而单纯的是向 Environment <ruby>问<rt>asks</rt></ruby> 就好。

![img](https://www.evernote.com/l/ABejjs0RksRL_LOo2jgoUk1bT54BBfMCqNAB/image.png "asks 可以将 e -> a 的函数变换成 Reader e a")

在运行时，可以提供给 Reader 的输入 Env fistname lastname。 ![img](https://www.evernote.com/l/ABc5cVh6zMND1KHY42FYTyRWzdfPcP4YYdEB/image.png)


<a id="orgd9a95d4"></a>

### do notation

这可能是你第一次见到 `do` 和 `<-`. 如果不是，随意跳过这节。

-   do 中所有 <- 的右边都是 `Reader Environment String` 类型
-   do 中的 return 返回类型也必须为 `Reader Environment String`
-   `asks firstName` 返回的是 `Reader Environment String` 类型， `<-` 可以理解成吧 monad `Reader Environment` 的内容放到左边的 f, 所以 f 的类型是 String。

看起来像命令式的语句，其实只是 `>>=` 的语法糖，但是明显用do可读性要高很多。

```haskell
helloworld = (asks firstName) >>=
  \f -> (asks lastName) >>=
       \l -> return "Hello " ++ f ++ l
```


<a id="orgaa3371f"></a>

## Writer

除了返回值，计算会需要产生一些额外的数据，比如 log

此时就需要一个 Writter，其返回值会是一个这样 `(result, log)` 的 tuple

限制是 log 的类型必须是个 <ruby>含幺半群<rt>monoid</rt></ruby>

```haskell
example :: Writer String String
example  = do
  tell "How are you?"
  tell "I'm fine thank you, and you?"
  return "Hehe Da~"

output :: (String, String)
output = runWriter example
-- ("Hehe Da~", "How are you?I'm fine thank you, and you?")
```

Writer 的定义更简单

```haskell
newtype Writer l a = Writer { runWriter :: (a,l) }
```

里面只是一个 tuple 而已

-   w 是 log
-   a 是 返回值

看看如何实现 Writer monad

```haskell
instance (Monoid w) => Monad (Writer w) where 
    return a             = Writer (a,mempty) 
    (Writer (a,l)) >>= f = let (a',l') = runWriter $ f a in
                           Writer (a',l `mappend` l')
```

-   return 不会有任何 log，l 是 monoid 的 mempty
-   f 的类型为 `a -> Writer l a`
-   `runWriter $ f a` 返回 `(a, l)`

![img](https://www.evernote.com/l/ABeB64fSK2BO27_IffFrMrIYjglJrp5rb5sB/image.png)

所以在 >>= 时，我们先把 f a 返回的 Writer run了，然后把两次 log `mappend` 起来。

![img](https://www.evernote.com/l/ABeLJJ_cN0JJa5PqDPVlKk4Tt0oAvBKnxf4B/image.png)


<a id="org0995559"></a>

## State

跟名字就看得出来 State monad 是为了处理状态。虽然函数式编程不应该有状态，不然会引用透明性。但是，state monad并不是在计算过程中修改状态，而是通过描述这种变化，然后需要时在运行返回最终结果。这一点跟 Reader 和 Writer 这两个看起来是副作用的 IO 是一样的。

先看下 State 类型的定义

```haskell
newtype State s a = State { runState :: s -> (a, s) }
```

可以看到 State 只包含一个 从旧状态 s 到新状态 s 和返回值 a 的 Tuple 的函数。

通过实现 Monad，State 就可以实现命令式编程中的变量的功能。

```haskell
instance Monad (State s) where 
  return a        = State $ \s -> (a,s)
  (State x) >>= f = State $ \s -> let (v,s') = x s in
                                 runState (f v) s'
```

return 很简单，就不用解释了。

![img](https://www.evernote.com/l/ABdBcGXH7T9FDoePyOg564ey9Kg7kndHtNUB/image.png)

x 类型是 `s -> (a, s)` ,所以 x s 之后会返回 结果和状态。也就是运行当前 State，把结果 v 传给函数 f，返回的 State 再接着上次状态运行。

![img](https://www.evernote.com/l/ABdHal0u69ZNBqHpPWJp-Dc6KC_yvQ1t3tsB/image.png "State x >>= f 后runState的数据流（啊啊啊，画歪了，感觉需要脉动一下）")

使用起来也很方便，State 提供 `get` `put` `moidfy` 三个方便的函数可以生成修改状态的State monad

```haskell
import Control.Monad.Trans.State.Strict
test :: State Int Int
test = do
  a <- get
  modify (+1)
  b <- get
  return (a + b)

main = print $ show $ runState test 3
-- (7, 4)
```


<a id="org3638020"></a>

## Expection


<a id="orgff354b5"></a>

## Cont


<a id="org6bebeac"></a>

## Summary

第二部分食用部分也讲完了， 不知是否以及大致了解了monad的尿性各种基本玩法呢？通过这些常用的基本的 monad instance，解决命令式编程中的一些简单问题应该是够了。

不过，接下来还有更变态的猫，就先叫她 ~~搞基~~ 猫呢好了。

-   👉 [第三部分：<ruby>搞基猫呢<rt> Advanced Monads</rt></ruby>](./part3.md)

当然我又还没空全部写完，如果还有很多人<ruby>预定<rt>只要998</rt></ruby> Gumroad 上的 <script src="https:https://gumroad.com/js/gumroad.js"></script><a class="gumroad-button" href="https:https://gum.co/grokking-monad" target="_blank">Grokking Monad</a> 电子书的话，我可能会稍微写得快一些。毕竟，写了也没人感兴趣也怪浪费时间的。不过，我猜也没几个人能看到这一行，就当是我又自言自语吧，怎么又突然觉得自己好分裂，诶~，为什么我要说又？


<a id="orgae307f0"></a>

## References

-   <https:https://wiki.haskell.org/All_About_Monads>
-   <http:https://dev.stephendiehl.com/hask/>

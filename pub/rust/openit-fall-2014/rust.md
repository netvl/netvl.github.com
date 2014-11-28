class: center, middle

# Rust: абстракции и безопасность, совершенно бесплатно

.right[Владимир Матвеев  
vmatveev@qubell.com]

---

## Rust

* компилируемый (LLVM)
* быстрый
* безопасный
* со статической типизацией
* с выводом типов
* с бесплатными абстракциями
* с функциональными элементами
* минимальный рантайм (либо его отсутствие)
* ...

---

## Кроссплатформенность

LLVM => множество платформ

Официально поддерживаются:

* Linux
* Mac OS X
* Win32/Win64

Также работает под Android и iOS

---

## Место

Близко к железу, но большой простор для ошибок:

* С
* С++

Высокоуровневые, безопасные, но дают меньше контроля:

* Java
* Haskell
* Python
* Ruby
* JS
* Go
* ...

---

## Классы ошибок

В C++:

* Висящие указатели
* Доступ за границами массивов
* Утечки памяти
* Переполнение буфера
* Use-after-free
* Гонки данных
* Iterator invalidation

В Java:

* NullPointerException
* ConcurrentModificationException
* Утечки памяти (!)

---

## Выводы

* Просто «Best practices» недостаточно
* Безопасность с помощью идиом/гайдов не обеспечить
* Компилятор должен сам отклонять небезопасные программы

---

## Решение

* Rust обеспечивает безопасность работы с памятью с помощью мощного статического анализа
* Нет явного управления памятью, компилятор отслеживает аллокации и деаллокации
* Если программа компилируется, то она работает с памятью безопасно
* Zero-cost abstractions, как в С++
* Вывод типов сильно помогает как при написании, так и при чтении

---

## История

* Появился как личный проект
* С 2009 спонсируется Mozilla Research
* Первый анонс в 2010 году
* В 2011 компилирует себя
* В 2012 выходит версия 0.1
* ...

---

## Будущее

* Начало 2015 - релиз первой стабильной версии
* Стабилизация языка и API
* Центральный репозиторий пакетов
* Множество отложенных фич
  + Типы высшего порядка
  + Ещё более продвинутые макросы
  + Вычисления при компиляции
  + ...

---
class: center, middle

# Типы данных

---

## Встроенные типы

* C фиксированным размером:  
  `i8, i32, i64, u8, u32, u64, f32, f64`
* С платформозависимым размером:
  `int, uint`
* Массивы и строки:
  `[u8, ..16], [i16], str`
* Кортежи:
  `(), (u16,), (f64, f64)`
* Ссылки:
  `&mut T, &T`
* «Сырые» указатели:
  `*mut T, *const T`
* Указатели на функции:  
  `fn(u32, u32), extern "C" fn(u16) -> u16`
* ~~Замыкания:
  `|int, int| -> int, proc(f64) -> u64`~~

---

## Срезы

```rust
let array: [u8, ..16] = [0, ..16];
let slice: &[u8] = &array;
println!("{}", slice.len());  // 16
```

Строковые срезы (и вообще строки) всегда в UTF-8:

```rust
let s: &str = "abcde";
let str_buf: String = s.into_string();
```

---

## Структуры

Обычные:
```rust
struct Point {
    x: f64,
    y: f64
}
```

Tuple structs:
```rust
struct Point(f64, f64);
```

Newtypes:
```rust
struct Distance(uint);
```

---

layout: true

## Перечисления

---

Как в C:
```rust
enum Direction {
    North, West = 123,
    South = 324, East
}
```

Как в Haskell:
```rust
enum Option<T> {
    Some(T),
    None
}
```

---

С вариантами-структурами:
```rust
enum Event {
    KeyPress { keycode: uint, modifiers: uint },
    MouseMove { x: u32, y: u32 },
    ...
}
```


---
layout: false
class: center, middle

# Основные элементы языка

---


С-подобный синтаксис
```rust
fn main() {
    for i in range(0, 10) {
        println!("Hello world!");
    }
}
```

Всё — выражения
```rust
let m = if x % 2 == 0 { "even" } else { "odd" };
```

Вывод типов
```rust
fn take_int(x: int) { ... }
let x = 10;
take_int(x);  // x is inferred as int
```

---

## Циклы

```rust
let (mut x, mut y) = (random_int(), random_int());
loop {
    x += 3;
    if x < y { continue; }
    y -= 3;
    if x > y { break; }
}
```

```rust
let mut n = 0;
while n < 100 {
    n += 1;
    if n % 5 == 2 { n += 13; }
}
```

---
layout: true

## Сопоставление с образцом

---

`switch` как в C:
```rust
let x: uint = 10;
let name = match x {
    1 => "one",
    2 => "two",
    _ => "many"
};
```

Деструктуризация как в Haskell:
```rust
let mut f = File::open("/tmp/input");
match f.read_to_end() {
    Ok(content) => println!("{} bytes", content.len()),
    Err(e) => println!("Error: {}", e)
}
```

---

Для срезов:

```rust
let x = [1, 2, 3, 4];
match x.as_slice() {
    [1, x, ..rest] => {
        println!("2nd: {}, all others: {}", x, rest);
    }
    _ => println!("Something else")
}
```

При объявлении переменных и параметров:

```rust
fn sum_tuple((x, y): (int, int)) -> int { 
    x + y
}
```

---

`if let`, `while let` из Swift:

```rust
if let Some(r) = from_str::<int>("12345") {
    println!("String \"12345\" is {}", r);
}
```

```rust
while let Ok(token) = next_token() {
    println!("Next token: {}", token);
}
```

---
layout: false

## Функции

```rust
fn multiply(left: uint, right: uint) -> uint {
    left + right
}

#[no_mangle]
pub extern fn visible_from_c(arg: u32) -> u32 {
    arg + arg
}
```

---

## Методы

```rust
struct Counter {
    base: u64
}

impl Counter {
    fn new(base: u64) -> Counter {
        Counter { base: base }
    }

    fn next(&mut self) -> u64 {
        let t = self.base;
        self.base += 1;
        return t;
    }
}

let mut counter = Counter::new(10);
println!("{} -> {} -> {}", 
         counter.next(), counter.next(), counter.next());
```

---
class: center, middle

# Полиморфизм

---

## Дженерики

Как шаблоны в C++:

```rust
enum Option<T> {
    None, Some(T)
}

fn unwrap_or<T>(opt: Option<T>, default: T) -> T {
    match opt {
        Some(value) => value,
        None => default
    }
}

println!("{}", unwrap_or(Some(10), 20));  // 10
println!("{}", unwrap_or(None, 30));      // 30
```

---

## Дженерики

```rust
enum Option<T> {
    None, Some(T)
}

impl<T> Option<T> {
    fn unwrap_or(self, default: T) -> T {
        match self {
            Some(value) => value,
            None => other
        }
    }
}

println!("{}", Some(10).unwrap_or(20));  // 10
println!("{}", None.unwrap_or(30));      // 30
```

---

## Трейты

Ограничения на ти́повые переменные:
```rust
trait Display {
    fn display(&self) -> String;
}

impl Display for int {
    fn display(&self) -> String { self.to_string() }
}

impl Display for String {
    fn display(&self) -> String { self.clone() }
}

fn print_twice<T: Display>(value: &T) {
    let s = value.display();
    println!("{} {}", value, value);
}
```

---

## Трейты

```rust
trait Add<RHS, Result> { fn add(&self, rhs: &RHS) -> Result; }
trait Mul<RHS, Result> { fn mul(&self, rhs: &RHS) -> Result; }

fn lin_map<T: Add<T, T>+Mul<T, T>>(a: T, b: T, x: T) -> T {
    a*x + b
}

// more readable
fn lin_map<T>(a: T, b: T, x: T) -> T
        where T: Add<T, T> + Mul<T, T> {
    a*x + b
}
```

---

## Трейты

Ср. с классами типов в Haskell:
```haskell
class Display a where
    display :: a -> String

class Add a rhs result where
    add :: a -> rhs -> result
```

А также реализации для произвольных типов, множественная диспетчеризация, ассоциированные типы, etc.

---

## Trait objects

```rust
fn print_slice<T: Show>(items: &[T]) {
    for item in items.iter() {
        println!("{} ", item);
    }
}

print_slice(&[1i, 2i, 3i]);   // ok
print_slice(&["a", "b"]);     // ok
print_slice(&[1i, 2i, "a"]);  // compilation error :( 
```

---

## Trait objects

Трейты как интерфейсы:

```rust
fn print_slice(items: &[&Show]) {
    for item in items.iter() {
        println!("{}", item);
    }
}

print_slice(&[&1i, &2i, &3i]);   // ok
print_slice(&[&"a", &"b"]);      // ok
print_slice(&[&1i, &2i, &"a"]);  // ok!
```

---

## Трейты

Zero-cost on-demand abstraction:

* Ограничения на дженерики — статический полиморфизм, мономорфизация, инлайнинг
* Trait objects — динамический полиморфизм, виртуальные таблицы, позднее связывание
* Cost is explicit — сразу видно, где именно появляется оверхед

---
class: middle, center

# Владение данными

---

## Ownership and borrowing

Владение и заимствование — ключевые концепции Rust

С помощью статических проверок на их основе компилятор способен предотвратить 
огромное число ошибок управления ресурсами: use-after-free, double-free, iterator
invalidation, data races

Владение данными основывается на теории *линейных типов* (linear types). Авторы Rust
вдохновлялись языками Clean и Cyclone; см. также `unique_ptr` в C++

---

## Ownership

```C
{
    int *x = malloc(sizeof(int));

    // do stuff
    *x = 5;

    free(x);
}
```
--

```rust
{
    let x = box 5;
}
```

---

## Ownership

```rust
fn add_one(mut num: Box<int>) {
    *num += 1;
}

let x = box 5i;

add_one(x);

*println!("{}", x);  // ! error: use of moved value: x
```

Move-семантика в действии!

---

## Ownership

```rust
fn add_one(mut num: Box<int>) -> Box<int> {
    *num += 1;
    num
}

let x = box 5i;

let y = add_one(x);

println!("{}", y);  // 6
```

---

## Copy

Некоторые типы реализуют трейт `Copy`; они автоматически копируются
вместо перемещения:

```rust
let x: int = 10;
let y = x;
println!("{}", x);
```

---

## RAII

Владение данными + move semantics + деструкторы = безопасный RAII

```rust
{
    let mut f = File::open(&Path::new("/some/path")).unwrap();
    // work with file ...
} // f's destructor is called here
  // (unless it is moved somewhere else)
```

Но move semantics подразумевает передачу права владения, что 
возможно далеко не всегда:

```rust
let mut f = File::open(&Path::new("/some/path")).unwrap();
let buf = [0u8, ..128];
f.read(buf).unwrap();
*println!("{}", buf);  // ! use of moved value: buf
```

---

## Borrowing

Владелец данных может предоставить к ним доступ с помощью *ссылок*:

```rust
fn with_one(num: &int) -> int {
    *num + 1
}

let x = box 5i;

println!("{}", with_one(&x));  // 6
```

---

## Borrowing

* `&T` — разделяемые/иммутабельные (shared/immutable)
* `&mut T` — неразделяемые/мутабельные (exclusive/mutable)

```rust
let x = 10i;
let p1 = &x;
let p2 = &x;  // ok
```

```rust
let mut x = 10i;
let p1 = &mut x;
*let p2 = &x;  // ! cannot borrow x as immutable because 
*             // ! it is also borrowed as mutable
```

```rust
let mut x = 10i;
let p1 = &mut x;
*let p2 = &mut x;  // ! cannot borrow x as mutable 
*                 // ! more than once at a time
```

---

## Borrowing and mutability

«Эксклюзивность» мутабельных ссылок исключает очень большой класс
ошибок вида use-after-free (и не только):
```rust
let mut v: Vec<int> = vec![1, 2];
let e = &v[0];
*v.push(3);  // reallocates the vector, moving its contents
*           // ! cannot borrow v as mutable because 
*           // ! it is also borrowed as immutable

```

```rust
let mut num = box 5i;
let e = &*num;
*num = box 6i;  // ! cannot assign to num because it is borrowed
```

```rust
let mut v = vec![1i, 2, 3];
for &e in v.iter() {
    println!("{}", e);
*    if e == 2 { v.push(-42); }  // ! cannot borrow v as mutable
}
```

---

## Borrowing and mutability

Отсутствие неожиданностей:

```rust
fn do_stuff(data: &mut BigData, should_process: || -> bool) {
    assert!(data.is_safe());
    if should_process() {
        unsafely_handle_data(data);
    }
}
```

---

## Borrowing and mutability

Наличие двух мутабельных ссылок позволяет реализовать `transmute()` 
(aka `reinterpret_cast`) в безопасном коде:

```rust
 fn my_transmute<T: Clone, U>(value: T, other: U) -> U {
     let mut x = Left(other);
     let y = match x {
         Left(ref mut y) => y,
         Right(_) => panic!()
     };
     x = Right(value);
     y.clone()
 }
```

---

## Lifetimes

«Наивное» заимствование может вызвать проблемы:
1. создаётся ресурс `X`;
2. на ресурс `X` берётся ссылка `a`;
3. ресурс `X` уничтожается;
4. к \[уничтоженному\] ресурсу `X` осуществляется доступ через ссылку `a`.

Use after free, доступ к закрытому файлу, etc.

Решение — **статически** обеспечить невозможность 4 перед 3.

---

## Lifetimes

С каждой ссылкой ассоциирован параметр — время жизни того объекта,
на который она указывает. Компилятор статически проверяет, что каждая ссылка
всегда «живёт» меньше, чем исходный объект:

```rust
fn make_ref<'a>() -> &'a int {
    let x = 10i;
*   &x  // ! x does not live long enough
}
```

```rust
fn first_and_last<'a>(slice: &'a [T]) -> (&'a T, &'a T) {
    (&slice[0], &slice[slice.len()-1])
}
```

```rust
fn first_and_last(slice: &[T]) -> (&T, &T) {  // identical
    (&slice[0], &slice[slice.len()-1])
}
```

---

## Lifetimes

Lifetime-параметры можно ассоциировать с областями видимости:

```rust
let x;
{
    let n = 5i;
*   x = &n;  // ! n does not live long enough
}
println!("{}", *x);
```

---

## Lifetimes

Lifetime-параметры «заражают» типы:

```rust
struct AnIntReference<'a> {
    r: &'a int
}

enum MaybeOwned<'a> {
    Owned(String),
    Slice(&'a str)
}
```

---

## Lifetimes

Специальный идентификатор `'static` обозначает время жизни всей программы:

```rust
static ANSWER: int = 42;

fn print_static_int_only(r: &'static int) {  // '
    println!("{}", *r);
}

fn main() {
    print_static_int_only(&ANSWER);  // ok
    
    let r = 21;
*   print_static_int_only(&r); // ! r does not live long enough
}
```

```rust
const MESSAGE: &'static str = "Hello world!";
```

---

## Shared ownership

В рамках одного потока — подсчёт ссылок:

```rust
use std::rc::Rc;
{
    let r = Rc::new(vec![1, 2, 3]);
    let r2 = r.clone();
    println!("{}", *r);
    println!("{}", *r2);
}  // both references go out of scope, Vec is destroyed
```

---
class: center, middle

# Многопоточность

---

## Потоки

Создаются функцией `spawn()`:

```rust
spawn(move || {  // unboxed closure
    println!("Hello from other thread!");
});
```

Потоки — это потоки ОС.

---

## Каналы

Общение между потоками происходит через каналы:

```rust
let (tx, rx) = channel();
spawn(move || {
    tx.send(4u + 6);
    tx.send(5u + 7);
});
println!("{}, {}", rx.recv(), rx.recv());
```

---

## Shared state

Данные «без ссылок внутри» разделяемые с помощью `Arc`:

```rust
use std::sync::Arc;

let data = Arc::new(vec![1u, 2, 3]);

let for_thread = data.clone();
spawn(move || {
    println!("From spawned thread: {}", *for_thread);
});

println!("From main thread: {}", *data);
```

---

## Mutable shared state

За счёт системы типов использование синхронизации *обязательно*. Таким образом,
исключаются гонки данных  
(data races):

```rust
use std::sync::{Arc, Mutex};

let data = Arc::new(Mutex::new(vec![1u, 2, 3]));

let for_thread = data.clone();
spawn(move || {
    let mut guard = for_thread.lock();
    guard.push(4);
    println!("{}", *guard);
});

let mut guard = data.lock();
guard.push(5);
println!("{}", *guard);
```

---
class: center, middle

# Unsafe

---

## Что это такое

`unsafe`-блоки и `unsafe`-функции:

```rust
unsafe fn from_raw_buf<'a, T>(p: &'a *const T, 
                              n: uint) -> &'a [u8] {
    std::mem::transmute(std::raw::Slice {
        data: *p,
        len: n
    })
}
```

```rust
fn fill_buffer<R: Reader>(r: R, size: uint) -> Vec<u8> {
    let v = Vec::with_capacity(size);
    unsafe { v.set_len(size) };
    r.read(v.as_mut_slice()).unwrap();
    v
}
```

---

## Unsafe-операции

* разыменование «сырого» указателя:
  ```rust
  let n = 10i;
  let p = &n as *const int;
  println!("{}", unsafe { *p });
  ```
* чтение/запись статической мутабельной переменной:
  ```rust
  static mut COUNTER: u32 = 10;
  unsafe { COUNTER += 32 };
  println!("{}", unsafe { COUNTER });
  ```
* вызов `unsafe`-функции:
  ```rust
  extern { fn perror(); }
  unsafe { perror(); }
  ```
---

## Когда это нужно

* ещё больше производительности
* абстракции
* взаимодействие с внешними библиотеками

Другими словами — очень редко!

---

class: center, middle

# Инфраструктура

---

## Встроенные модульные тесты

```rust
#[test]
fn test_something() {
    assert_eq!(true, true);
}

#[bench]
fn test_perf(b: &mut Bencher) {
    b.iter(|| {
        do_something();
    });
}
```

---

## Единица компиляции — crate

```rust
pub mod a {
    mod b {
        // ...
    }
    pub mod c {
        // ...
    }
}

mod d {
    // ...
}
```

На выходе — готовый бинарник (библиотека или executable)

---

## Менеджер сборки — Cargo

* разработан Yehuda Katz — автором Bundler
* сборка и управление проектом:
  - отслеживание зависимостей
  - компиляция зависимостей, как на Rust, так и на C
  - компиляция проекта
  - запуск тестов, модульных и интеграционных
  - генерация пакетов и их деплой в репозиторий
* reproducible builds

---

## crates.io — центральный репозиторий

* Открылся совсем недавно
* Предназначен, в основном, для стабильных релизов
* 400 пакетов спустя полторы недели
* Ядро будущей экосистемы

---

class: center, middle

# Проекты на Rust

---

* Servo — https://github.com/servo/
  - исследовательский браузерный движок
  - активно развивается, уже проходит какие-то тесты
  - ~100000 строк
* rustc — https://github.com/rust-lang/rust
  - сам компилятор Rust
  - самый старый крупный проект
  - ~400000 строк
* Cargo — https://github.com/rust-lang/cargo
  - менеджер сборки
  - один из наиболее новых проектов, idiomatic style
  - ~30000 строк

---

* Piston — https://github.com/PistonDevelopers
  - коллекция проектов, связанных с разработкой игр
  - байндинги к OpenGL и другим графическим (и не только) библиотекам
  - игровой движок
  - GUI-библиотека
* Zinc — https://zinc.rs
  - ARM-стек
  - эффективный и безопасный фреймворк для RTOS-систем
  - ~17000 строк
* Iron — https://ironframework.org/
  - наиболее популярный веб-фреймворк (есть и другие!)
  - middleware-based
  - вместе с HTTP-библиотекой Hyper ~8000 строк

---

class: center, middle

# Ссылки

---

* Общее
  - http://www.rust-lang.org/
  - https://github.com/rust-lang
  - http://reddit.com/r/rust
  - http://discuss.rust-lang.org/
  - https://crates.io/
  - irc.mozilla.org - #rust, #rust-internals, #cargo, #servo, ...
  - https://groups.google.com/forum/#!forum/rust-russian
* Документация и туториалы
  - Stackoverflow по тегу \[rust\]
  - http://doc.rust-lang.org/guide.html
  - https://github.com/rust-lang/rust/wiki/Docs
  - https://rustbyexample.com/

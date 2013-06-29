## Trailing whitespace in selectors

```css
.foo bar > baz    { rules: go here; }
//            ^^^^ this break the parser
```

* Owing to the way my `sepBy` handler uses whitespace

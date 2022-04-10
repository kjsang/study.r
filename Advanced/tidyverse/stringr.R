pacman::p_load(
  tidyverse, magrittr,
  stringr
)
string1 <- "문자열입니다"
string2 <- "문자열 내에 \'인용문\'이 사용된 경우 작은 따옴표를 사용하세요."
escape_string <- "작은 따옴표나 큰따옴표 문자를 문자열에 포함하려면 벗어나기를 뜻하는 역슬래시를 사용하면 된다. \'이렇게!\' \n띄어쓰기는 역슬래쉬 앤이다. 그리고 탭은 역슬래시 t 로 한다. \t같은 원리로 역슬래시 문자를 포함하려면 \\ 라고 입력해야 한다."
escape_string
# 문자열의 원시형태를 보려면 writeLines를 쓰자
writeLines(escape_string)
?"'"

# \n	newline
# \r	carriage return
# \t	tab
# \b	backspace
# \a	alert (bell)
# \f	form feed
# \v	vertical tab
# \\	backslash \
# \'	ASCII apostrophe '
# \"	ASCII quotation mark "
# \`	ASCII grave accent (backtick) `
# \nnn	character with given octal code (1, 2 or 3 digits)
# \xnn	character with given hex code (1 or 2 hex digits)
# \unnnn	Unicode character with given code (1--4 hex digits)
# \Unnnnnnnn	Unicode character with given code (1--8 hex digits)

str_c("x", "y", sep = ", ")
str_c("나는", c("김주상", "주찬양"), "입니다.", sep = " ")
str_c(c("x", "y", "z", "a"), collapse = ", ") # 문자열 벡터를 하나의 문자열로 합치기

x <- c("apple", "banana", "melon")
str_sub(x, 1, 3) # 문자, 스타트, 마지막
# 짧아도 가능한 만큼만 반환한다.
# 앞글자를 대문자로 바꾸기? 갸꿀
str_sub(x, 1, 1) <- str_to_upper(str_sub(x,1,1))

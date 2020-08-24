# userdb

REST API сервис, с функционалом:
- регистрации;
- авторизации;
- смены пароля пользователя;
- получения списка пользователей.

## Примеры использования:
### Регистрация:
```
curl -H "Content-Type: application/json" -d '{"user":"it is user", "password":"user password"}' http://localhost:8080/registration -v
*   Trying ::1...
* TCP_NODELAY set
* connect to ::1 port 8080 failed: В соединении отказано
*   Trying 127.0.0.1...
* TCP_NODELAY set
* Connected to localhost (127.0.0.1) port 8080 (#0)
> POST /registration HTTP/1.1
> Host: localhost:8080
> User-Agent: curl/7.58.0
> Accept: */*
> Content-Type: application/json
> Content-Length: 49
>
* upload completely sent off: 49 out of 49 bytes
< HTTP/1.1 200 OK
< content-length: 38
< content-type: application/json; charset=utf-8
< date: Mon, 24 Aug 2020 06:43:12 GMT
< server: Cowboy
<
* Connection #0 to host localhost left intact
{"description":"Success registration"}
```

### Авторизация:
```
curl -H "Content-Type: application/json" -d '{"user":"it is user", "password":"user password"}' http://localhost:8080/authorization -v
*   Trying ::1...
* TCP_NODELAY set
* connect to ::1 port 8080 failed: В соединении отказано
*   Trying 127.0.0.1...
* TCP_NODELAY set
* Connected to localhost (127.0.0.1) port 8080 (#0)
> POST /authorization HTTP/1.1
> Host: localhost:8080
> User-Agent: curl/7.58.0
> Accept: */*
> Content-Type: application/json
> Content-Length: 49
>
* upload completely sent off: 49 out of 49 bytes
< HTTP/1.1 200 OK
< content-length: 39
< content-type: application/json; charset=utf-8
< date: Mon, 24 Aug 2020 06:43:43 GMT
< server: Cowboy
< set-cookie: session_id=#Ref<0.816725826.551288833.37280>; Version=1
<
* Connection #0 to host localhost left intact
{"description":"Success authorization"}
```

### Получения списка пользователей:
```
 curl -H "Content-Type: application/json" -H "cookie: session_id=#Ref<0.816725826.551288833.37280>; Version=1" -d '{"offset":0,"limit":50}' http://localhost:8080/get_users_list -v
*   Trying ::1...
* TCP_NODELAY set
* connect to ::1 port 8080 failed: В соединении отказано
*   Trying 127.0.0.1...
* TCP_NODELAY set
* Connected to localhost (127.0.0.1) port 8080 (#0)
> POST /get_users_list HTTP/1.1
> Host: localhost:8080
> User-Agent: curl/7.58.0
> Accept: */*
> Content-Type: application/json
> cookie: session_id=#Ref<0.816725826.551288833.37280>; Version=1
> Content-Length: 23
>
* upload completely sent off: 23 out of 23 bytes
< HTTP/1.1 200 OK
< content-length: 83
< content-type: application/json; charset=utf-8
< date: Mon, 24 Aug 2020 06:44:42 GMT
< server: Cowboy
<
* Connection #0 to host localhost left intact
{"list":["it is user","it is user 2","it is user 3","it is user 4","it is user 5"]}
```

### Изменение пароля пользователя:
```
 curl -H "Content-Type: application/json" -H "cookie: session_id=#Ref<0.816725826.551288833.37280>; Version=1" -d '{"password":"user password","new_password":"new user password"}' http://localhost:8080/change_user_password -v
*   Trying ::1...
* TCP_NODELAY set
* connect to ::1 port 8080 failed: В соединении отказано
*   Trying 127.0.0.1...
* TCP_NODELAY set
* Connected to localhost (127.0.0.1) port 8080 (#0)
> POST /change_user_password HTTP/1.1
> Host: localhost:8080
> User-Agent: curl/7.58.0
> Accept: */*
> Content-Type: application/json
> cookie: session_id=#Ref<0.816725826.551288833.37280>; Version=1
> Content-Length: 63
>
* upload completely sent off: 63 out of 63 bytes
< HTTP/1.1 200 OK
< content-length: 52
< content-type: application/json; charset=utf-8
< date: Mon, 24 Aug 2020 06:45:35 GMT
< server: Cowboy
<
* Connection #0 to host localhost left intact
{"description":"User password changed successfully"}
```

P.S. Тестирование проводилось на mysql-cleint-5.7 и mysql-server-5.7

## rebar3

Для сборки проекта используется rebar3. Предполагается, что вы уже умеете им пользоваться :)
Можно скопировать уже скомпилированный файл:
```
wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3
```
Добавляем путь к файлу в переменную окружения $PATH:
```
./rebar3 local install
```
```
echo "export PATH=$PATH:~/.cache/rebar3/bin" >> ~/.bashrc
```
Взято из https://habr.com/ru/post/319950/

## Сборка и запуск

Для сборки приложения сначала подтягиваем зависимости командой:
```
rebar3 get-deps
```
Затем компилируем:
```
rebar3 compile
```

Перед использованием нужно поставить mysql клиент, т.к. через него
делается создание пользователя, базы и таблицы mysql. Выполняем команду:
```
sudo mysql < ./priv/mysql-prepare.sql
```
В результате данной команды для сервиса в mysql будут созданы:
* пользователь userdb;
* база userdb;
* таблица user.

P.S. Предполагается, что вы находитесь в папке сервиса.

По умолчанию сервис подключается к mysql серверу расположенному на localhost,
настройки находятся в файле конфигурации по пути:
```
./config/sys.config
```

Запускаем  командой:
```
rebar3 console
```

## Тестирование

Перед запуском тестов нужно выполить команду:
```
sudo mysql < ./test/priv/mysql-prepare.sql
```

P.S. Предполагается, что вы находитесь в папке сервиса.

В результате данной команды для тестов в mysql будут созданы:
* пользователь userdb_test;
* база userdb_test.

По умолчанию во время тестов сервис будет подключается к mysql серверу расположенному
на localhost, настройки находятся в файле конфигурации по пути:
./test/config/sys.config

Запускаем тесты командами:
```
rebar3 eunit
rebar3 ct
```

## Релиз

Релиз собираем командой:
```
rebar3 release
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling userdb
===> Assembling release userdb-0.1.0...
===> Warnings generating release:
*WARNING* public_key: Source code not found: 'OTP-PUB-KEY'.erl
*WARNING* public_key: Source code not found: 'PKCS-FRAME'.erl
===> Release successfully assembled: _build/default/rel/userdb
```

После этого сервис можно запускать командами:
```
./_build/default/rel/userdb/bin/userdb console
```
или:
```
./_build/default/rel/userdb/bin/userdb daemon
```

Так же можно собрать архив( и в дальнейшем использовать уже его):
```
rebar3 tar
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling userdb
===> Assembling release userdb-0.1.0...
===> Warnings generating release:
*WARNING* public_key: Source code not found: 'OTP-PUB-KEY'.erl
*WARNING* public_key: Source code not found: 'PKCS-FRAME'.erl
===> Release successfully assembled: _build/default/rel/userdb
===> Building release tarball userdb-0.1.0.tar.gz...
===> Tarball successfully created: _build/default/rel/userdb/userdb-0.1.0.tar.gz
```

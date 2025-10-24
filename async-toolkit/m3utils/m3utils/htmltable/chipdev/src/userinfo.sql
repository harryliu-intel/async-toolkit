create table userinfo (
        uid             int4 ,
        constraint uid_pk primary key (uid),

        name            varchar(64) not null,
        title           varchar(64),

        status          varchar(12) not null,   -- check it has right type
        constraint status_legal check (   status = 'user'
                                       OR status = 'admin'
                                      ),
        pass            varchar(20)            -- encrypted password
);

create table session_tbl (   
       uid               int4 not null,
       constraint uid_fk foreign key (uid) references userinfo (uid),

       session_key       varchar(64) not null,
       constraint sk_uq unique (session_key),  -- code depends on this

       host_ip           varchar(20) not null,
       expires           timestamp not null
);

insert into userinfo (uid, name  , title             ,   status, pass) values
                     (0  , 'root', 'Supreme Overlord', 'admin', ''  );

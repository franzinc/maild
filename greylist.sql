create database greylist;

use greylist;

create table triples (ip int unsigned not null, 
       sender text not null, 
       receiver text not null, 
       createtime bigint unsigned not null,
       blockexpire bigint unsigned not null,
       expire bigint unsigned not null,
       blocked int unsigned not null, 
       passed int unsigned not null);

create index ind1 on triples (ip, sender(50), receiver(50));

create table optout (receiver text not null);
create table optin (receiver text not null);

create index ind2 on optout (receiver(50));
create index ind3 on optin (receiver(50));

create table whitelist (sender text not null, 
			receiver text not null, 
			source text not null,
			expire bigint);
create index ind4 on whitelist (receiver(50), sender(50));
create index ind5 on whitelist (source(25));
create index ind6 on whitelist (expire);

grant all on greylist.* to greylist identified by "greypasswd";

--
-- PostgreSQL database dump
--

--LOAD  '/usr/share/postgresql/contrib/ltree.sql'
SET statement_timeout = 0;
SET client_encoding = 'SQL_ASCII';
SET standard_conforming_strings = off;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET escape_string_warning = off;

--
-- Name: plpgsql; Type: PROCEDURAL LANGUAGE; Schema: -; Owner: postgres
--

CREATE OR REPLACE PROCEDURAL LANGUAGE plpgsql;


ALTER PROCEDURAL LANGUAGE plpgsql OWNER TO postgres;

SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: Email; Type: TABLE; Schema: public; Owner: forum; Tablespace: 
--



CREATE TABLE "Email" (
    id integer NOT NULL,
    email character varying NOT NULL,
    "user" bigint,
    verkey character varying
);


ALTER TABLE public."Email" OWNER TO forum;

--
-- Name: Email_id_seq; Type: SEQUENCE; Schema: public; Owner: forum
--

CREATE SEQUENCE "Email_id_seq"
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public."Email_id_seq" OWNER TO forum;

--
-- Name: Email_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: forum
--

ALTER SEQUENCE "Email_id_seq" OWNED BY "Email".id;


--
-- Name: Email_id_seq; Type: SEQUENCE SET; Schema: public; Owner: forum
--

SELECT pg_catalog.setval('"Email_id_seq"', 1, false);


--
-- Name: Post; Type: TABLE; Schema: public; Owner: forum; Tablespace: 
--

CREATE TABLE "Post" (
    id integer NOT NULL,
    parent bigint,
    node_path ltree,
    "user" bigint,
    created timestamp without time zone DEFAULT now() NOT NULL,
    title character varying NOT NULL,
    body character varying NOT NULL
);


ALTER TABLE public."Post" OWNER TO forum;

--
-- Name: Post_id_seq; Type: SEQUENCE; Schema: public; Owner: forum
--

CREATE SEQUENCE "Post_id_seq"
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public."Post_id_seq" OWNER TO forum;

--
-- Name: Post_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: forum
--

ALTER SEQUENCE "Post_id_seq" OWNED BY "Post".id;


--
-- Name: Post_id_seq; Type: SEQUENCE SET; Schema: public; Owner: forum
--

SELECT pg_catalog.setval('"Post_id_seq"', 1, false);

CREATE UNIQUE INDEX idx_post_node_path_btree_idx ON "Post" USING btree(node_path);
CREATE INDEX idx_post_node_path_gist_idx ON "Post" USING gist(node_path);

CREATE OR REPLACE FUNCTION get_calculated_post_node_path(param_post_id bigint)
  RETURNS ltree AS
$$
SELECT  CASE WHEN p.parent IS NULL THEN p.id::text::ltree 
            ELSE get_calculated_post_node_path(p.parent)  || p.id::text END
    FROM "Post" As p
    WHERE p.id = $1;
$$
  LANGUAGE sql;


/** trigger to maintain node when parent or item id changes or record is added  
    We revised this since the first version because the order of the short-circuiting is not predictable so TG_OP needs a nested IF.
    Also some other minor simplifications
**/
CREATE OR REPLACE FUNCTION trig_update_post_node_path() RETURNS trigger AS
$$
BEGIN
  IF TG_OP = 'UPDATE' THEN
        IF (COALESCE(OLD.parent,0) != COALESCE(NEW.parent,0)  OR  NEW.id != OLD.id) THEN
            -- update all nodes that are children of this one including this one
            UPDATE "Post" SET node_path = get_calculated_post_node_path(id) 
                WHERE OLD.node_path  @> "Post".node_path;
        END IF;
  ELSIF TG_OP = 'INSERT' THEN
        UPDATE "Post" SET node_path = get_calculated_post_node_path(NEW.id) WHERE "Post".id = NEW.id;
  END IF;
  
  RETURN NEW;
END
$$
LANGUAGE 'plpgsql' VOLATILE;

CREATE TRIGGER trig01_update_post_node_path AFTER INSERT OR UPDATE OF id, parent
   ON "Post" FOR EACH ROW
   EXECUTE PROCEDURE trig_update_post_node_path();

--
-- Name: User; Type: TABLE; Schema: public; Owner: forum; Tablespace: 
--

CREATE TABLE "User" (
    id integer NOT NULL,
    ident character varying NOT NULL,
    password character varying
);


ALTER TABLE public."User" OWNER TO forum;

--
-- Name: User_id_seq; Type: SEQUENCE; Schema: public; Owner: forum
--

CREATE SEQUENCE "User_id_seq"
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public."User_id_seq" OWNER TO forum;

--
-- Name: User_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: forum
--

ALTER SEQUENCE "User_id_seq" OWNED BY "User".id;


--
-- Name: User_id_seq; Type: SEQUENCE SET; Schema: public; Owner: forum
--

SELECT pg_catalog.setval('"User_id_seq"', 1, false);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: forum
--

ALTER TABLE "Email" ALTER COLUMN id SET DEFAULT nextval('"Email_id_seq"'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: forum
--

ALTER TABLE "Post" ALTER COLUMN id SET DEFAULT nextval('"Post_id_seq"'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: forum
--

ALTER TABLE "User" ALTER COLUMN id SET DEFAULT nextval('"User_id_seq"'::regclass);


--
-- Data for Name: Email; Type: TABLE DATA; Schema: public; Owner: forum
--

COPY "Email" (id, email, "user", verkey) FROM stdin;
\.


--
-- Data for Name: Post; Type: TABLE DATA; Schema: public; Owner: forum
--

COPY "Post" (id, parent, "user", created, title, body) FROM stdin;
\.


--
-- Data for Name: User; Type: TABLE DATA; Schema: public; Owner: forum
--

COPY "User" (id, ident, password) FROM stdin;
\.


--
-- Name: Email_pkey; Type: CONSTRAINT; Schema: public; Owner: forum; Tablespace: 
--

ALTER TABLE ONLY "Email"
    ADD CONSTRAINT "Email_pkey" PRIMARY KEY (id);


--
-- Name: Post_pkey; Type: CONSTRAINT; Schema: public; Owner: forum; Tablespace: 
--

ALTER TABLE ONLY "Post"
    ADD CONSTRAINT "Post_pkey" PRIMARY KEY (id);


--
-- Name: UniqueEmail; Type: CONSTRAINT; Schema: public; Owner: forum; Tablespace: 
--

ALTER TABLE ONLY "Email"
    ADD CONSTRAINT "UniqueEmail" UNIQUE (email);


--
-- Name: UniqueUser; Type: CONSTRAINT; Schema: public; Owner: forum; Tablespace: 
--

ALTER TABLE ONLY "User"
    ADD CONSTRAINT "UniqueUser" UNIQUE (ident);


--
-- Name: User_pkey; Type: CONSTRAINT; Schema: public; Owner: forum; Tablespace: 
--

ALTER TABLE ONLY "User"
    ADD CONSTRAINT "User_pkey" PRIMARY KEY (id);


--
-- Name: Email_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: forum
--

ALTER TABLE ONLY "Email"
    ADD CONSTRAINT "Email_user_fkey" FOREIGN KEY ("user") REFERENCES "User"(id);


--
-- Name: Post_parent_fkey; Type: FK CONSTRAINT; Schema: public; Owner: forum
--

ALTER TABLE ONLY "Post"
    ADD CONSTRAINT "Post_parent_fkey" FOREIGN KEY (parent) REFERENCES "Post"(id);


--
-- Name: Post_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: forum
--

ALTER TABLE ONLY "Post"
    ADD CONSTRAINT "Post_user_fkey" FOREIGN KEY ("user") REFERENCES "User"(id);


--
-- Name: public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;

INSERT INTO "Post"(parent,title,body)
VALUES (NULL, 'Paper','lolol'),
(1, 'Recycled','lolol'),
(2, '20 lb','lolol'),
(2, '40 lb','lolol'),
(1, 'Non-Recycled','lolol'),
(5, '20 lb','lolol'),
(5, '40 lb','lolol'),
(5, 'Scraps','lolol');

UPDATE "Post" SET parent = Null WHERE id = 5;

SELECT * FROM "Post";
--
-- PostgreSQL database dump complete
--


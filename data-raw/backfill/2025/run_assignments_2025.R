# generate 2025 data for query
library(tidyverse)
library(grunID)

# make sure con is for production for this section
con_prod <- gr_db_connect()


# create grunID table - ONLY RUN ONCE; HERE FOR RECORD KEEPING ------------

create_gtseq_table_query <- glue::glue_sql("
CREATE TABLE public.gtseq_results (
	id int4 GENERATED ALWAYS AS IDENTITY( INCREMENT BY 1 MINVALUE 1 MAXVALUE 2147483647 START 1 CACHE 1 NO CYCLE) NOT NULL,
	sample_id varchar(255) NOT NULL,
	gtseq_chr28_geno varchar(50) NULL,
	pop_structure_id varchar(50) NULL,
	cv_fall numeric(4, 3) NULL,
	cv_late_fall numeric(4, 3) NULL,
	cv_spring numeric(4, 3) NULL,
	cv_winter numeric(4, 3) NULL,
	tributary varchar(100) NULL,
	buttefall int4 NULL,
	frh_fall numeric(4, 3) NULL,
	frh_sp int4 NULL,
	mill_deer_fall numeric(4, 3) NULL,
	san_joaquin_fall numeric(4, 3) NULL,
	butte_sp int4 NULL,
	mill_deer_sp numeric(4, 3) NULL,
	coleman_f varchar(50) NULL,
	sac_win int4 NULL,
	season int4 NULL,
	created_by text DEFAULT CURRENT_USER NULL,
	created_at timestamp DEFAULT CURRENT_TIMESTAMP NULL,
	CONSTRAINT gtseq_results_pkey PRIMARY KEY (id),
	CONSTRAINT gtseq_results_cv_fall_check CHECK (((cv_fall >= (0)::numeric) AND (cv_fall <= (1)::numeric))),
	CONSTRAINT gtseq_results_cv_late_fall_check CHECK (((cv_late_fall >= (0)::numeric) AND (cv_late_fall <= (1)::numeric))),
	CONSTRAINT gtseq_results_cv_spring_check CHECK (((cv_spring >= (0)::numeric) AND (cv_spring <= (1)::numeric))),
	CONSTRAINT gtseq_results_cv_winter_check CHECK (((cv_winter >= (0)::numeric) AND (cv_winter <= (1)::numeric))),
	CONSTRAINT gtseq_results_frh_fall_check CHECK (((frh_fall >= (0)::numeric) AND (frh_fall <= (1)::numeric))),
	CONSTRAINT gtseq_results_mill_deer_fall_check CHECK (((mill_deer_fall >= (0)::numeric) AND (mill_deer_fall <= (1)::numeric))),
	CONSTRAINT gtseq_results_mill_deer_sp_check CHECK (((mill_deer_sp >= (0)::numeric) AND (mill_deer_sp <= (1)::numeric))),
	CONSTRAINT gtseq_results_san_joaquin_fall_check CHECK (((san_joaquin_fall >= (0)::numeric) AND (san_joaquin_fall <= (1)::numeric)))
);"
)


res <- DBI::dbExecute(con_prod, create_gtseq_table_query)

# foreign keys
foreign_key_update <- glue::glue_sql(
"ALTER TABLE public.gtseq_results ADD CONSTRAINT gtseq_results_sample_id_fkey FOREIGN KEY (sample_id) REFERENCES public.sample(id) ON DELETE RESTRICT ON UPDATE CASCADE;"
)

res <- DBI::dbExecute(con_prod, foreign_key_update)



# check
tbl(con_prod, "gtseq_results") |>
  glimpse()


# add gtseq results to db -------------------------------------------------------

gtseq_2025 <- read_gtseq("data-raw/backfill/2025/JPE_2025_Reanalysis_10-2025_summary.tsv")

gtseq_2025 |>
  glimpse()

insert_gtseq_raw_results(con_prod, gtseq_2025)

# checks
new_ids <- gtseq_2025 |>
  pull(SampleID)

tbl(con_prod, "sample") |>
  filter(id %in% new_ids) |>
  collect() |>
  glimpse()


# create view for final run assignment ------------------------------------

create_view_query <- glue::glue_sql(
  "-- DROP VIEW IF EXISTS final_run_assignment;
CREATE VIEW final_run_assignment AS
SELECT COALESCE(gri.sample_id, gr.sample_id) AS sample_id,
       s.datetime_collected, s.fork_length_mm, s.field_run_type_id,
       gri.run_type_id, gr.gtseq_chr28_geno, gr.pop_structure_id,
       gr.cv_fall, gr.cv_late_fall, gr.cv_spring, gr.cv_winter,
       gr.tributary, gr.buttefall, gr.frh_fall, gr.frh_sp,
       gr.mill_deer_fall, gr.san_joaquin_fall, gr.butte_sp,
       gr.mill_deer_sp, gr.coleman_f, gr.sac_win
FROM genetic_run_identification gri
FULL JOIN gtseq_results gr ON gri.sample_id = gr.sample_id
LEFT JOIN sample s ON COALESCE(gri.sample_id, gr.sample_id) = s.id;"
)

res <- DBI::dbExecute(con_prod, create_view_query)

with drug AS (SELECT
       ROW_NUMBER() over (ORDER BY concept_code) AS
       precedence,
       concept_name AS category_name,
       concept_id AS category_id
  FROM @cdmDatabaseSchema.concept
  WHERE vocabulary_id='ATC' AND concept_class_id= '@ATC'
)
SELECT distinct
  concept_id AS concept_id_2, concept_name as drug_name,
  first_value(coalesce(category_id, 0)) over
  (partition by concept_id ORDER BY precedence nulls last) as category_id,
  first_value(coalesce(category_name, 'Other Drug'))
  over (partition by concept_id ORDER BY precedence nulls last)
  AS category_name
FROM @cdmDatabaseSchema.concept
LEFT JOIN (
  SELECT descendant_concept_id, category_id, category_name, precedence
  FROM @cdmDatabaseSchema.concept_ancestor
  JOIN drug on ancestor_concept_id=category_id
) d on descendant_concept_id=concept_id
WHERE concept_id in (@drugConceptIds)

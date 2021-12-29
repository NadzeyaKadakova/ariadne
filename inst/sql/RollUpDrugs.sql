with drug as (SELECT ROW_NUMBER() over (order by concept_code) as
       precedence,
       concept_name as category_name,
       concept_id as category_id
  FROM @cdmDatabaseSchema.concept
  WHERE vocabulary_id='ATC' and concept_class_id='ATC 2nd'
)
SELECT distinct
  concept_id as concept_id_2, concept_name as drug_name,
  first_value(coalesce(category_id, 0)) over
  (partition by concept_id order by precedence nulls last) as category_id,
  first_value(coalesce(category_name, 'Other Drug'))
  over (partition by concept_id order by precedence nulls last) as category_name
FROM @cdmDatabaseSchema.concept
left JOIN (
  SELECT descendant_concept_id, category_id, category_name, precedence
  FROM @cdmDatabaseSchema.concept_ancestor
  JOIN drug on ancestor_concept_id=category_id
) d on descendant_concept_id=concept_id
WHERE concept_id in (@drugConceptIds)

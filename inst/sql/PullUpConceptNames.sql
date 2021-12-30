SELECT concept_name,
       concept_id
FROM @cdmDatabaseSchema.concept
WHERE concept_id IN (@covariateIds)

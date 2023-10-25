SELECT
  project_course_user.ProjectCourseConfigId project_id,
  project_course_config.Name project_name,
  project_course_user.OrganizationUserId user_id,
  project_course_user.Progress project_progress
FROM
  iquizoo_business_db.project_course_user
  INNER JOIN iquizoo_business_db.project_course_config ON project_course_config.Id = project_course_user.ProjectCourseConfigId
WHERE
  project_course_config.Id = ?

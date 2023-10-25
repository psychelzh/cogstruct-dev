SELECT
  ProjectCourseConfigId project_id,
  OrganizationUserId user_id,
  Progress project_progress
FROM
  iquizoo_business_db.project_course_user
WHERE
  ProjectCourseConfigId = ?

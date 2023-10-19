SELECT DISTINCT
    pcc.Id project_id,
    pcc.Name project_name,
    pcc.OrganizationId org_id,
    crs.Period course_period,
    crs.CreateTime course_date,
    c.Id game_id
FROM
    iquizoo_content_db.content c
    INNER JOIN iquizoo_content_db.course_child_config ccc ON ccc.ContentId = c.Id AND ccc.Deleted <> 1 AND c.Deleted <> 1
    INNER JOIN iquizoo_content_db.course_child cc ON cc.Id = ccc.ChildCourseId AND cc.Deleted <> 1
    INNER JOIN iquizoo_content_db.course crs ON crs.Id = cc.CourseId AND crs.Deleted <> 1
    INNER JOIN iquizoo_business_db.project_course_resource pcr ON pcr.CourseId = crs.Id AND pcr.Deleted <> 1
    INNER JOIN iquizoo_business_db.project_course_config pcc ON pcc.ProjectCourseResourceId = pcr.Id AND pcc.Deleted <> 1
WHERE
    c.Name REGEXP 'Q' OR c.VersionName REGEXP 'Q' AND pcc.Name NOT REGEXP '测试|test' AND pcc.Name NOT REGEXP '^[0-9]+$';

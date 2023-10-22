SELECT
    bo.Id organization_id,
    bo.Name organization_name,
    pcc.Id project_id,
    pcc.Name project_name,
    c.Id course_id,
    c.Name course_name,
    c.Period course_period,
    c.CreateTime course_date,
    c2.Id game_id,
    c2.ContentType game_type
FROM
    iquizoo_business_db.project_course_config pcc
    INNER JOIN iquizoo_content_db.course c ON c.Id = pcc.CourseId AND c.Deleted <> 1 AND pcc.Deleted <> 1
    INNER JOIN iquizoo_content_db.course_child cc ON cc.CourseId = c.Id AND cc.Deleted <> 1
    INNER JOIN iquizoo_content_db.course_child_config ccc ON ccc.ChildCourseId = cc.Id AND ccc.Deleted <> 1
    INNER JOIN iquizoo_content_db.content c2 ON c2.Id = ccc.ContentId AND c2.ContentType <> 4 AND c2.Deleted <> 1
    INNER JOIN iquizoo_user_db.base_organization bo ON bo.Id = pcc.OrganizationId AND bo.Deleted <> 1
WHERE
    bo.Id IN (
        SELECT org_base.Id
        FROM
            iquizoo_user_db.base_organization org_base
            INNER JOIN iquizoo_user_db.base_organization org_sup
                ON org_base.SuperiorId = org_sup.Id AND org_base.Deleted <> 1 AND org_sup.Deleted <> 1
            WHERE org_sup.Name = 'CAMP'
    )
    AND c.CourseSeriesId = (
        SELECT cs.Id
        FROM iquizoo_content_db.course_series cs
        WHERE cs.Name = 'CAMP'
    );

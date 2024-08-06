package com.be.rebook.members.repository;

import com.be.rebook.members.entity.Universities;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.Optional;

public interface UniversitiesRepository extends JpaRepository<Universities, Long> {
    @Query("SELECT u FROM Universities u WHERE u.university LIKE %:unvToSearch%")
    List<Universities> searchByUniversity(@Param("unvToSearch") String unvToSearch);

    Optional<Universities> findByUniversity(String university);

    Optional<Universities> findByUnvId(Long id);
}

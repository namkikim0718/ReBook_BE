package com.be.rebook.members.repository;

import com.be.rebook.members.entity.Majors;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.Optional;


public interface MajorsRepository extends JpaRepository<Majors, Long> {
    @Query("SELECT m FROM Majors m WHERE m.major LIKE %:majorToSearch%")
    List<Majors> searchByMajor(@Param("majorToSearch") String majorToSearch);

    Optional<Majors> findByMajor(String major);

    Optional<Majors> findByMajorId(String majorId);
}

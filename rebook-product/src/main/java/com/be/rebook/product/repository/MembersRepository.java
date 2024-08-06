package com.be.rebook.product.repository;

import com.be.rebook.product.entity.Members;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface MembersRepository extends JpaRepository<Members, Long> {
    Boolean existsByMemberId(Long memberId);

    Boolean existsByUsername(String username);

    Optional<Members> findByUsername(String username);

    Optional<Members> findByMemberId(Long memberId);
}

package com.be.rebook.members.repository;

import com.be.rebook.members.entity.RefreshTokens;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

public interface RefreshTokensRepository extends JpaRepository<RefreshTokens, Long> {
    Boolean existsByRefresh(String refresh);

    @Transactional
    void deleteByRefresh(String refresh);
    List<RefreshTokens> findByUsername(String username);
}
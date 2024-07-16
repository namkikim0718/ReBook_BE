package com.be.rebook.members.repository;

import com.be.rebook.members.entity.RefreshTokens;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface RefreshTokensRepository extends JpaRepository<RefreshTokens, Long> {
    Boolean existsByRefresh(String refresh);

    List<RefreshTokens> findByUsername(String username);
}
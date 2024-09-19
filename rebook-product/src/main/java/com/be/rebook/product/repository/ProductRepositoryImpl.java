package com.be.rebook.product.repository;

import com.be.rebook.product.dto.ProductRequestDTO;
import com.be.rebook.product.entity.Product;
import com.querydsl.core.types.OrderSpecifier;
import com.querydsl.core.types.dsl.BooleanExpression;
import com.querydsl.jpa.JPQLQuery;
import com.querydsl.jpa.impl.JPAQueryFactory;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;

import java.time.LocalDateTime;
import java.util.List;

import static com.be.rebook.product.entity.QProduct.product;

@RequiredArgsConstructor
public class ProductRepositoryImpl implements ProductRepositoryCustom {

    private final JPAQueryFactory jpaQueryFactory;

    @Override
    public Page<Product> findProductsByFilter(ProductRequestDTO.ProductFilterDTO productFilterDTO, Pageable pageable) {

        // 가격순 정렬과 커서 기반 필터링 조건 추가
        JPQLQuery<Product> query = jpaQueryFactory.selectFrom(product)
                .where(
                        universityContains(productFilterDTO.getUniversity()),
                        majorContains(productFilterDTO.getMajor()),
                        titleContains(productFilterDTO.getTitle()),
                        priceBetween(productFilterDTO.getMinPrice(), productFilterDTO.getMaxPrice()),
                        priceCursorBasedPagination(productFilterDTO.getLastPrice(), productFilterDTO.getLastProductId(), productFilterDTO.getSortOrder()) // 커서 기반 필터링 추가
                )
                .orderBy(getOrderSpecifier(productFilterDTO.getSortOrder())) // 가격순 정렬
                .limit(pageable.getPageSize());

        List<Product> products = query.fetch();

        long total = query.fetchCount(); // 전체 레코드 수
        return new PageImpl<>(products, pageable, total); // 총 개수를 포함한 페이지 반환
    }

    private BooleanExpression universityContains(String university) {
        return university != null && !university.isEmpty() ? product.university.contains(university) : null;
    }

    private BooleanExpression majorContains(String major) {
        return major != null && !major.isEmpty() ? product.major.contains(major) : null;
    }

    private BooleanExpression titleContains(String title) {
        return title != null && !title.isEmpty() ? product.title.contains(title) : null;
    }

    private BooleanExpression priceBetween(Integer minPrice, Integer maxPrice) {
        if (minPrice != null && maxPrice != null) {
            return product.price.between(minPrice, maxPrice);
        } else if (minPrice != null) {
            return product.price.goe(minPrice);
        } else if (maxPrice != null) {
            return product.price.loe(maxPrice);
        } else {
            return null;
        }
    }

    // 가격을 기반으로 커서 페이지네이션 구현 (가격과 ID로 필터링)
    private BooleanExpression priceCursorBasedPagination(Integer lastPrice, Long lastProductId, String sortOrder) {
        if (lastPrice != null && lastProductId != null) {
            if ("asc".equalsIgnoreCase(sortOrder)) {
                return product.price.gt(lastPrice) // 가격이 커서 가격보다 큰 경우
                        .or(product.price.eq(lastPrice).and(product.id.gt(lastProductId))); // 가격이 동일하면 ID로 추가 필터링
            } else if ("desc".equalsIgnoreCase(sortOrder)) {
                return product.price.lt(lastPrice) // 가격이 커서 가격보다 작은 경우
                        .or(product.price.eq(lastPrice).and(product.id.lt(lastProductId))); // 가격이 동일하면 ID로 추가 필터링
            } else {
                return product.id.lt(lastProductId);
            }
        }

        return null;
    }

    private OrderSpecifier<?> getOrderSpecifier(String order) {
        if ("asc".equalsIgnoreCase(order)) {
            return product.price.asc();
        } else if ("desc".equalsIgnoreCase(order)) {
            return product.price.desc();
        } else if ("recent".equalsIgnoreCase(order)) {
            return product.id.desc();
        } else {
            return product.id.desc(); // 기본값: 최신순 정렬
        }
    }
}
